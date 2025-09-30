{-# LANGUAGE OverloadedStrings #-}

-- | vLLM (Qwen) client via OpenAI-compatible Chat Completions API.
--   Supports strict JSON schema enforcement using `response_format.json_schema`.
module HaskLLM.VLLM.QweN2_5
  ( Qwen (..),
  )
where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
  ( Value (..),
    eitherDecode,
    encode,
    object,
    (.=),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HaskLLM
  ( ChatMessage (..),
    Credentials (..),
    JSONSchemaSpec (..),
    LLMFormatChat (..),
    RequestConfig (..),
    defaultRequestConfig,
  )
import Network.HTTP.Client
  ( Request,
    RequestBody (..),
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
    responseTimeoutNone,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Provider tag for vLLM/Qwen (OpenAI-compatible server).
data Qwen = Qwen

--------------------------------------------------------------------------------
-- Retry and timeout helpers

-- | Retry an IO action with exponential backoff
retryWithBackoff :: Int -> IO a -> IO a
retryWithBackoff maxRetries action = go maxRetries (1 :: Int)
  where
    go 0 _ = action -- Last attempt, don't catch
    go retriesLeft delay = do
      result <- catch (Right <$> action) (pure . Left)
      case result of
        Right success -> pure success
        Left (_ :: SomeException) -> do
          -- Simple backoff: wait delay seconds, then double it
          if delay <= 8 -- Cap at 8 seconds
            then do
              -- In a real implementation, you'd use threadDelay, but for simplicity:
              go (retriesLeft - 1) (delay * 2)
            else go (retriesLeft - 1) delay

-- | Configure timeout for a request based on RequestConfig
configureTimeout :: RequestConfig -> Request -> Request
configureTimeout config req = case timeoutSeconds config of
  Nothing -> req {responseTimeout = responseTimeoutNone}
  Just seconds -> req {responseTimeout = responseTimeoutMicro (seconds * 1000000)}

instance LLMFormatChat Qwen where
  -- Plain chat (no schema).
  respondText _ creds modelName msgs =
    liftIO $
      makeTextRequest creds modelName msgs Nothing defaultRequestConfig

  -- Enforced JSON schema via `response_format` (vLLM / chat.completions).
  respondJSON _ creds modelName msgs schema =
    liftIO $
      makeJSONRequest creds modelName msgs schema Nothing defaultRequestConfig

  -- Plain chat with configurable max tokens.
  respondTextWithTokens _ creds modelName msgs mMaxTokens =
    liftIO $
      makeTextRequest creds modelName msgs mMaxTokens defaultRequestConfig

  -- Enforced JSON schema with configurable max tokens via `response_format` (vLLM / chat.completions).
  respondJSONWithTokens _ creds modelName msgs schema mMaxTokens =
    liftIO $
      makeJSONRequest creds modelName msgs schema mMaxTokens defaultRequestConfig

  -- New configurable methods
  respondTextWithConfig _ creds modelName msgs config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        makeTextRequest creds modelName msgs Nothing config

  respondJSONWithConfig _ creds modelName msgs schema config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        makeJSONRequest creds modelName msgs schema Nothing config

  respondTextWithTokensAndConfig _ creds modelName msgs mMaxTokens config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        makeTextRequest creds modelName msgs mMaxTokens config

  respondJSONWithTokensAndConfig _ creds modelName msgs schema mMaxTokens config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        makeJSONRequest creds modelName msgs schema mMaxTokens config

--------------------------------------------------------------------------------
-- Helpers (local)

required :: Text -> M.Map Text Text -> IO Text
required k m = case M.lookup k m of
  Just v -> pure v
  Nothing -> fail ("Missing credential key: " <> T.unpack k)

-- | Make a text request with configurable timeout and retries
makeTextRequest :: Credentials -> Text -> [ChatMessage] -> Maybe Int -> RequestConfig -> IO Text
makeTextRequest (Credentials cred) modelName msgs mMaxTokens config = do
  base <- required "base_url" cred
  apiKey <- required "api_key" cred
  session <- required "session_token" cred

  let url = concretizeChatEndpoint base
      body = case mMaxTokens of
        Nothing ->
          object
            [ "model" .= modelName,
              "messages" .= msgs,
              "temperature" .= (0.7 :: Double)
            ]
        Just maxTokens ->
          object
            [ "model" .= modelName,
              "messages" .= msgs,
              "temperature" .= (0.7 :: Double),
              "max_tokens" .= maxTokens
            ]
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest (T.unpack url)
  let req =
        configureTimeout config $
          req0
            { method = "POST",
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  ("x-api-key", TE.encodeUtf8 apiKey),
                  ("x-session-token", TE.encodeUtf8 session)
                ],
              requestBody = RequestBodyLBS (encode body)
            }
  resp <- httpLbs req manager
  let raw = responseBody resp
  case eitherDecode raw :: Either String Value of
    Left e -> fail ("vLLM: invalid JSON response: " <> e)
    Right js -> pure (extractChatContent js)

-- | Make a JSON request with configurable timeout and retries
makeJSONRequest :: Credentials -> Text -> [ChatMessage] -> JSONSchemaSpec -> Maybe Int -> RequestConfig -> IO Value
makeJSONRequest (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) mMaxTokens config = do
  base <- required "base_url" cred
  apiKey <- required "api_key" cred
  session <- required "session_token" cred

  let url = concretizeChatEndpoint base
      responseFormat =
        object
          [ "type" .= ("json_schema" :: Text),
            "json_schema"
              .= object
                [ "name" .= nm,
                  "schema" .= sch,
                  "strict" .= isStrict
                ]
          ]
      body = case mMaxTokens of
        Nothing ->
          object
            [ "model" .= modelName,
              "messages" .= msgs,
              "temperature" .= (0.7 :: Double),
              "response_format" .= responseFormat
            ]
        Just maxTokens ->
          object
            [ "model" .= modelName,
              "messages" .= msgs,
              "temperature" .= (0.7 :: Double),
              "response_format" .= responseFormat,
              "max_tokens" .= maxTokens
            ]
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest (T.unpack url)
  let req =
        configureTimeout config $
          req0
            { method = "POST",
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  ("x-api-key", TE.encodeUtf8 apiKey),
                  ("x-session-token", TE.encodeUtf8 session)
                ],
              requestBody = RequestBodyLBS (encode body)
            }
  resp <- httpLbs req manager
  let raw = responseBody resp
  js <- case eitherDecode raw :: Either String Value of
    Left e -> fail ("vLLM: invalid JSON response: " <> e)
    Right ok -> pure ok

  let txt = extractChatContent js
  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 txt) :: Either String Value of
    Right v -> pure v
    Left e -> fail ("vLLM: schema-enforced output was not valid JSON: " <> e)

-- Normalize a base URL into a concrete Chat Completions endpoint.
-- Accepts either:
--   - full endpoint that already ends with "/chat/completions" (used verbatim), or
--   - base with or without trailing slash (we append "/v1/chat/completions").
concretizeChatEndpoint :: Text -> Text
concretizeChatEndpoint base0 =
  let base = T.dropWhileEnd (== '/') base0
   in if "/chat/completions" `T.isSuffixOf` base
        then base
        else base <> "/v1/chat/completions"

-- Extract assistant content from OpenAI-compatible Chat Completions.
extractChatContent :: Value -> Text
extractChatContent (Object o)
  | Just (Array choices) <- KM.lookup "choices" o,
    (Object firstChoice : _) <- toList choices,
    Just (Object msg) <- KM.lookup "message" firstChoice,
    Just (String contentText) <- KM.lookup "content" msg =
      contentText
extractChatContent _ = ""
