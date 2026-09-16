module OpenAIAccountingTest (spec) where

import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Network.HTTP.Client (
  HttpException (..),
  HttpExceptionContent (ConnectionTimeout),
  Response,
  checkResponse,
  defaultManagerSettings,
  defaultRequest,
  httpLbs,
  newManager,
  parseRequest,
 )
import Network.HTTP.Types (Status, status200, status400, status503)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import Text.Pandoc (Pandoc (..), nullMeta)

import HaskLLM
import HaskLLM.FallbackLLM (FallbackProvider (..), ProviderConfig (..))
import HaskLLM.Internal (extractChatUsage)
import HaskLLM.OpenAI.GPT5 (runToolLoop)
import HaskLLM.OpenAI.Request
import HaskLLM.OpenAI.Retry (OpenAIHttpError (..))
import HaskLLM.PandocChat (respondPandocChatWithTokensDetailed)
import HaskLLM.Tools (Tool (..), ToolSpec (..), aggregateUsage)

-- The generic adapters need a local endpoint. Use the real Responses transport
-- and parsers against a loopback server, not a mock LLM response method.
data LocalResponses = LocalResponses AttemptObserver (IO (Response LBS.ByteString))

instance LLMFormatChat LocalResponses where
  respondText prov creds model messages =
    responseContent <$> respondTextDetailed prov creds model messages Nothing defaultRequestConfig
  respondJSON prov creds model messages schema =
    responseContent <$> respondJSONDetailed prov creds model messages schema Nothing defaultRequestConfig
  respondTextDetailed (LocalResponses observe transport) _ model _ _ config = liftIO do
    response <- requestOpenAI observe model (maxRetries config) transport
    pure $ LLMResponse (extractResponsesText response) (extractResponsesUsage response) model "openai"
  respondJSONDetailed prov creds model messages _ tokens config = do
    response <- respondTextDetailed prov creds model messages tokens config
    content <- liftIO $ parseJSONContent $ responseContent response
    pure response {responseContent = content}

-- A real HTTP boundary with a finite script. Unexpected retries remain visible
-- in the request count instead of silently receiving another successful reply.
withResponses :: [(Status, LBS.ByteString)] -> (IO (Response LBS.ByteString) -> IO Int -> IO a) -> IO a
withResponses responses action = do
  state <- newIORef (0 :: Int, responses)
  testWithApplication
    ( pure \_ send -> do
        (number, (status, body)) <- atomicModifyIORef' state \(count, remaining) ->
          let (reply, rest) = case remaining of
                [] -> ((status503, "unexpected request"), [])
                first : others -> (first, others)
           in ((count + 1, rest), (count + 1, reply))
        send $ responseLBS status [("x-request-id", BS8.pack $ "req_" <> show number)] body
    )
    \port -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest $ "http://127.0.0.1:" <> show port <> "/v1/responses"
      action
        (httpLbs request {checkResponse = \_ _ -> pure ()} manager)
        (fst <$> readIORef state)

recording :: IO (AttemptObserver, IO [AttemptObservation])
recording = do
  observations <- newIORef []
  pure
    ( \observation -> atomicModifyIORef' observations \previous -> (previous <> [observation], ()),
      readIORef observations
    )

usage :: Value
usage =
  object
    [ "input_tokens" .= (120 :: Int),
      "output_tokens" .= (30 :: Int),
      "total_tokens" .= (150 :: Int),
      "input_tokens_details"
        .= object
          ["cached_tokens" .= (40 :: Int), "cache_write_tokens" .= (60 :: Int)],
      "output_tokens_details" .= object ["reasoning_tokens" .= (10 :: Int)]
    ]

responseBody :: Text -> Value
responseBody content =
  object
    [ "id" .= ("resp_paid" :: Text),
      "model" .= ("gpt-5.6-luna-2026-09-01" :: Text),
      "status" .= ("completed" :: Text),
      "output_text" .= content,
      "usage" .= usage
    ]

spec :: Spec
spec = describe "OpenAI accounting" do
  it "preserves the input breakdown without adding cache tokens to totals" do
    extractResponsesUsage (responseBody "ok")
      `shouldBe` Just (TokenUsage (Just 120) (Just 30) (Just 150) (Just 40) (Just 60) (Just 10) Nothing)

  it "preserves provider-billed cost alongside cache-write accounting" do
    Just chatUsage <- pure $ extractChatUsage $ object ["usage" .= object ["cost" .= (0.125 :: Double)]]
    Just openAIUsage <- pure $ extractResponsesUsage $ responseBody "ok"
    let combined = aggregateUsage [openAIUsage, chatUsage, chatUsage]
    cacheWriteTokens chatUsage `shouldBe` Nothing
    costUsd chatUsage `shouldBe` Just 0.125
    (combined >>= costUsd) `shouldBe` Just 0.25
    (combined >>= cacheWriteTokens) `shouldBe` Just 60
    (aggregateUsage [openAIUsage] >>= costUsd) `shouldBe` Nothing

  it "distinguishes missing usage, missing cache fields and reported zero" do
    let parse raw = either (error . ("bad fixture: " <>)) extractResponsesUsage $ eitherDecode raw
    map parse ["{}", "{\"usage\":null}", "{\"usage\":false}"]
      `shouldBe` replicate 3 Nothing
    map
      (parse >=> cacheWriteTokens)
      [ "{\"usage\":{\"input_tokens\":12}}",
        "{\"usage\":{\"input_tokens_details\":{\"cached_tokens\":4}}}",
        "{\"usage\":{\"input_tokens_details\":{\"cache_write_tokens\":0}}}",
        "{\"usage\":{\"input_tokens_details\":{\"cache_write_tokens\":null}}}"
      ]
      `shouldBe` [Nothing, Nothing, Just 0, Nothing]

  it "observes fenced JSON responses without rejecting or retrying them" do
    withResponses [(status200, encode $ responseBody "```json\n{}\n```")] \transport requests -> do
      (observe, recorded) <- recording
      response <-
        respondJSONDetailed
          (LocalResponses observe transport)
          (Credentials M.empty)
          "model"
          []
          (JSONSchemaSpec "test" (object []) True)
          Nothing
          defaultRequestConfig
      responseContent response `shouldBe` object []
      (responseUsage response >>= cacheWriteTokens) `shouldBe` Just 60
      length <$> recorded `shouldReturn` 1
      requests `shouldReturn` 1

  it "observes paid responses even when assistant JSON or Pandoc patches are invalid" do
    -- Invalid JSON, then valid JSON with the wrong Pandoc shape: both were billed.
    withResponses (map ((status200,) . encode . responseBody) ["not JSON", "{}"]) \transport requests -> do
      (observe, recorded) <- recording
      let provider = LocalResponses observe transport
          creds = Credentials M.empty
      respondJSONDetailed
        provider
        creds
        "requested-model"
        []
        (JSONSchemaSpec "test" (object []) True)
        Nothing
        defaultRequestConfig
        `shouldThrow` anyIOException
      respondPandocChatWithTokensDetailed
        provider
        creds
        "requested-model"
        (M.singleton "user" $ Pandoc nullMeta [])
        (Just [[]])
        Nothing
        `shouldThrow` anyIOException
      observations <- recorded
      map attemptRequestId observations `shouldBe` [Just "req_1", Just "req_2"]
      map attemptResponseId observations `shouldBe` replicate 2 (Just "resp_paid")
      map attemptModel observations `shouldBe` replicate 2 "gpt-5.6-luna-2026-09-01"
      map attemptResponseStatus observations `shouldBe` replicate 2 (Just "completed")
      map (attemptUsage >=> cacheWriteTokens) observations `shouldBe` replicate 2 (Just 60)
      requests `shouldReturn` 2

  it "observes every retried HTTP response before status handling, even on final failure" do
    withResponses
      [(status503, encode $ responseBody "retry"), (status400, "{\"error\":\"bad request\"}")]
      \transport requests -> do
        (observe, recorded) <- recording
        requestOpenAI observe "requested-model" 3 transport
          `shouldThrow` (\(OpenAIHttpError status _) -> status == 400)
        observations <- recorded
        map attemptOutcome observations `shouldBe` [HTTPResponse 503, HTTPResponse 400]
        map attemptModel observations `shouldBe` ["gpt-5.6-luna-2026-09-01", "requested-model"]
        map (attemptUsage >=> cacheWriteTokens) observations `shouldBe` [Just 60, Nothing]
        requests `shouldReturn` 2

  it "records undecodable envelopes as unknown usage without retrying them" do
    withResponses [(status200, "not an envelope")] \transport requests -> do
      (observe, recorded) <- recording
      requestOpenAI observe "model" 3 transport `shouldThrow` anyIOException
      observations <- recorded
      map attemptOutcome observations `shouldBe` [HTTPResponse 200]
      map attemptUsage observations `shouldBe` [Nothing]
      requests `shouldReturn` 1

  it "records unknown usage for each transport failure and rethrows the failure" do
    (observe, recorded) <- recording
    requestOpenAI
      observe
      "model"
      1
      (throwIO $ HttpExceptionRequest defaultRequest ConnectionTimeout)
      `shouldThrow` (\case HttpExceptionRequest _ ConnectionTimeout -> True; _ -> False)
    observations <- recorded
    length observations `shouldBe` 2
    map attemptUsage observations `shouldBe` [Nothing, Nothing]
    map attemptOutcome observations `shouldBe` replicate 2 (TransportFailure "ConnectionTimeout")

  it "keeps earlier tool usage when a follow-up fails without replaying handlers" do
    let call =
          object
            [ "usage" .= usage,
              "output"
                .= [ object
                       [ "type" .= ("function_call" :: Text),
                         "call_id" .= ("call_1" :: Text),
                         "name" .= ("count" :: Text),
                         "arguments" .= ("{}" :: Text)
                       ]
                   ]
            ]
    withResponses
      [(status200, encode call), (status503, "{}"), (status400, "{}")]
      \transport requests -> do
        (observe, recorded) <- recording
        handlers <- newIORef (0 :: Int)
        let tool = Tool
              (ToolSpec "count" "Count invocations" (object []) False)
              \(_ :: Value) -> do
                atomicModifyIORef' handlers \count -> (count + 1, ())
                pure "done"
        runToolLoop (const $ requestOpenAI observe "model" 1 transport) [tool] [] 3
          `shouldThrow` (\(OpenAIHttpError status _) -> status == 400)
        observations <- recorded
        map (attemptUsage >=> cacheWriteTokens) observations `shouldBe` [Just 60, Nothing, Nothing]
        readIORef handlers `shouldReturn` 1
        requests `shouldReturn` 3

  it "aggregates cache writes from each successful tool round exactly once" do
    let call =
          object
            [ "usage" .= usage,
              "output"
                .= [ object
                       [ "type" .= ("function_call" :: Text),
                         "call_id" .= ("call_1" :: Text),
                         "name" .= ("unknown" :: Text),
                         "arguments" .= ("{}" :: Text)
                       ]
                   ]
            ]
    withResponses
      [(status200, encode call), (status200, encode $ responseBody "done")]
      \transport _ -> do
        (observe, recorded) <- recording
        (_, usages) <- runToolLoop (const $ requestOpenAI observe "model" 0 transport) [] [] 3
        (aggregateUsage usages >>= cacheWriteTokens) `shouldBe` Just 120
        (aggregateUsage usages >>= cachedInputTokens) `shouldBe` Just 80
        (aggregateUsage usages >>= inputTokens) `shouldBe` Just 240
        length <$> recorded `shouldReturn` 2

  it "retains observations when fallback succeeds after a paid parsing failure" do
    withResponses
      (map ((status200,) . encode . responseBody) ["invalid JSON", "{}"])
      \transport requests -> do
        (observe, recorded) <- recording
        let config = ProviderConfig (LocalResponses observe transport) (Credentials M.empty)
        response <-
          respondJSONDetailed
            (FallbackProvider (config "primary") (config "secondary"))
            (Credentials M.empty)
            "ignored"
            []
            (JSONSchemaSpec "test" (object []) True)
            Nothing
            defaultRequestConfig
        responseContent response `shouldBe` object []
        length <$> recorded `shouldReturn` 2
        requests `shouldReturn` 2

  it "does not retry or fall back when an observer throws a retryable exception" do
    withResponses [(status200, encode $ responseBody "ok")] \transport requests -> do
      let config =
            ProviderConfig
              (LocalResponses (const $ throwIO $ OpenAIHttpError 503 mempty) transport)
              (Credentials M.empty)
              "model"
      response <-
        respondTextDetailed
          (FallbackProvider config config)
          (Credentials M.empty)
          "ignored"
          []
          Nothing
          defaultRequestConfig
      responseContent response `shouldBe` "ok"
      requests `shouldReturn` 1

  it "propagates observer cancellation through retries and fallback" do
    withResponses [(status200, encode $ responseBody "ok")] \transport requests -> do
      let config =
            ProviderConfig
              (LocalResponses (const $ throwIO ThreadKilled) transport)
              (Credentials M.empty)
              "model"
      respondTextDetailed
        (FallbackProvider config config)
        (Credentials M.empty)
        "ignored"
        []
        Nothing
        defaultRequestConfig
        `shouldThrow` (== ThreadKilled)
      requests `shouldReturn` 1
