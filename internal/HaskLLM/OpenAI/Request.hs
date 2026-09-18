-- | Responses API transport and decoding. Observe each attempt before a retry
-- or content parser can discard it; consumers own persistence and pricing.
module HaskLLM.OpenAI.Request (
  requestOpenAI,
  extractResponsesUsage,
  extractResponsesText,
  parseJSONContent,
) where

import Control.Exception (displayException, fromException, throwIO)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client (
  HttpException (..),
  Response,
  responseBody,
  responseHeaders,
  responseStatus,
 )
import Network.HTTP.Types.Status (statusCode)
import System.IO (hPutStrLn, stderr)

import HaskLLM (AttemptObservation (..), AttemptObserver, AttemptOutcome (..), TokenUsage (..))
import HaskLLM.Exception (trySync)
import HaskLLM.Internal (lookupNestedInt, stripCodeFence)
import HaskLLM.OpenAI.Retry (checkOpenAIResponse, retryOpenAIRequest)

-- | Only the transport/status check is retried. Observers and content parsers
-- must never cause another paid request. The transport must return non-2xx
-- responses rather than throwing them away in http-client's checkResponse.
requestOpenAI :: AttemptObserver -> Text -> Int -> IO (Response LBS.ByteString) -> IO Value
requestOpenAI observe model retries transport = do
  decoded <- retryOpenAIRequest retries do
    trySync transport >>= \case
      Left exception -> do
        -- HttpException's Show includes the request and its Authorization header.
        let reason = case fromException exception of
              Just (HttpExceptionRequest _ content) -> show content
              _ -> displayException exception
        notifyAttempt observe $
          AttemptObservation
            "openai"
            model
            Nothing
            Nothing
            Nothing
            (TransportFailure $ T.pack reason)
            Nothing
        throwIO exception
      Right response -> do
        let decoded = eitherDecode $ responseBody response
            envelope = either (const Nothing) Just decoded
        notifyAttempt observe $
          AttemptObservation
            { attemptProvider = "openai",
              attemptModel = fromMaybe model $ envelope >>= lookupText "model",
              attemptRequestId = TE.decodeUtf8With lenientDecode <$> lookup "x-request-id" (responseHeaders response),
              attemptResponseId = envelope >>= lookupText "id",
              attemptResponseStatus = envelope >>= lookupText "status",
              attemptOutcome = HTTPResponse $ statusCode $ responseStatus response,
              attemptUsage = envelope >>= extractResponsesUsage
            }
        _ <- checkOpenAIResponse response
        pure decoded
  either (fail . ("OpenAI: invalid JSON response: " <>)) pure decoded

notifyAttempt :: AttemptObserver -> AttemptObservation -> IO ()
notifyAttempt observe observation =
  trySync (observe observation) >>= \case
    Right () -> pure ()
    Left exception ->
      -- A broken logging sink must not turn an accounting failure into a retry.
      void $
        trySync $
          hPutStrLn stderr $
            "[haskllm] Attempt observer failed: " <> displayException exception

-- | Decode the assistant's JSON after its enclosing response has been observed.
parseJSONContent :: Text -> IO Value
parseJSONContent text =
  either
    (\err -> fail $ "OpenAI: schema-enforced output was not valid JSON: " <> err <> "\nRaw response text: " <> T.unpack text)
    pure
    (eitherDecode $ LBS.fromStrict $ TE.encodeUtf8 $ stripCodeFence text)

-- | Prefer the convenience output_text field; otherwise join message content.
extractResponsesText :: Value -> Text
extractResponsesText (Object envelope)
  | Just (String text) <- KM.lookup "output_text" envelope = text
  | Just (Array output) <- KM.lookup "output" envelope =
      T.intercalate
        "\n"
        [ text
        | Object item <- toList output,
          Just (Array content) <- [KM.lookup "content" item],
          Object block <- toList content,
          Just (String text) <- [KM.lookup "text" block]
        ]
extractResponsesText _ = ""

extractResponsesUsage :: Value -> Maybe TokenUsage
extractResponsesUsage (Object envelope) = do
  usage@(Object _) <- KM.lookup "usage" envelope
  pure
    TokenUsage
      { inputTokens = lookupNestedInt ["input_tokens"] usage,
        outputTokens = lookupNestedInt ["output_tokens"] usage,
        totalTokens = lookupNestedInt ["total_tokens"] usage,
        cachedInputTokens = lookupNestedInt ["input_tokens_details", "cached_tokens"] usage,
        cacheWriteTokens = lookupNestedInt ["input_tokens_details", "cache_write_tokens"] usage,
        reasoningTokens = lookupNestedInt ["output_tokens_details", "reasoning_tokens"] usage,
        costUsd = Nothing
      }
extractResponsesUsage _ = Nothing

lookupText :: K.Key -> Value -> Maybe Text
lookupText key (Object fields) = do
  String text <- KM.lookup key fields
  pure text
lookupText _ _ = Nothing
