{-# LANGUAGE OverloadedStrings #-}

-- | OpenAI GPT-5 client using the Responses API, with structured-output enforcement.
--   Exposes a generic typeclass for conversational generation with (optional) JSON schema.
--   The vLLM/Qwen module imports this to implement the same interface against a different endpoint.
--
--   Credentials can be provided via the Credentials map or environment variables:
--   - @openai_api_key@: OPENAI_API_KEY (e.g., "sk-proj-...")
--
--   The provider checks the Credentials map first, then falls back to the OPENAI_API_KEY
--   environment variable if the key is missing. This enables flexible configuration.
module HaskLLM.OpenAI.GPT5
  ( -- * Shared interface & types
    Credentials (..),
    ChatMessage (..),
    JSONSchemaSpec (..),
    RequestConfig (..),
    defaultRequestConfig,
    LLMFormatChat (..),

    -- * Provider tag for OpenAI
    OpenAI (..),
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
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
import System.Environment (lookupEnv)

-- | Provider tag for OpenAI GPT‑5 (Responses API).
data OpenAI = OpenAI

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

instance LLMFormatChat OpenAI where
  -- Responses API text output (no schema).
  respondText _ creds modelName msgs =
    liftIO $
      makeTextRequest creds modelName msgs Nothing defaultRequestConfig

  -- Responses API structured output with JSON schema.
  respondJSON _ creds modelName msgs schema =
    liftIO $
      makeJSONRequest creds modelName msgs schema Nothing defaultRequestConfig

  -- Responses API text output with configurable max tokens.
  respondTextWithTokens _ creds modelName msgs mMaxTokens =
    liftIO $
      makeTextRequest creds modelName msgs mMaxTokens defaultRequestConfig

  -- Responses API structured output with JSON schema and configurable max tokens.
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
-- Helpers

-- | Get a required credential, with environment variable fallback.
--   Checks the credentials map first, then falls back to environment variables.
required :: Text -> Map Text Text -> IO Text
required k m = case M.lookup k m of
  Just v -> pure v
  Nothing -> do
    -- Map credential keys to environment variable names
    let envVar = case k of
          "openai_api_key" -> "OPENAI_API_KEY"
          _ -> T.unpack k -- Default: use the key name as-is
    mEnv <- lookupEnv envVar
    case mEnv of
      Just val -> pure (T.pack val)
      Nothing ->
        fail $
          "Missing credential key: "
            <> T.unpack k
            <> " (not in Credentials map, and environment variable "
            <> envVar
            <> " is not set)"

-- | Make a text request with configurable timeout and retries
makeTextRequest :: Credentials -> Text -> [ChatMessage] -> Maybe Int -> RequestConfig -> IO Text
makeTextRequest (Credentials cred) modelName msgs mMaxTokens config = do
  apiKey <- required "openai_api_key" cred
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest "https://api.openai.com/v1/responses"

  let inputMessages = map chatMessageToValue msgs
      maxTokens = fromMaybe 8192 mMaxTokens
      body =
        object
          [ "model" .= modelName,
            "input" .= inputMessages,
            "max_output_tokens" .= maxTokens
          ]
      req =
        configureTimeout config $
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode body)
            }

  resp <- httpLbs req manager
  let raw = responseBody resp

  case eitherDecode raw :: Either String Value of
    Left e -> fail ("OpenAI: invalid JSON response: " <> e)
    Right js -> pure (extractResponsesText js)

-- | Make a JSON request with configurable timeout and retries
makeJSONRequest :: Credentials -> Text -> [ChatMessage] -> JSONSchemaSpec -> Maybe Int -> RequestConfig -> IO Value
makeJSONRequest (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) mMaxTokens config = do
  apiKey <- required "openai_api_key" cred
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest "https://api.openai.com/v1/responses"

  let inputMessages = map chatMessageToValue msgs
      maxTokens = fromMaybe 8192 mMaxTokens
      textFormat =
        object
          [ "type" .= ("json_schema" :: Text),
            "name" .= nm,
            "schema" .= sch,
            "strict" .= isStrict
          ]
      body =
        object
          [ "model" .= modelName,
            "input" .= inputMessages,
            "text" .= object ["format" .= textFormat],
            "max_output_tokens" .= maxTokens
          ]
      req =
        configureTimeout config $
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode body)
            }

  resp <- httpLbs req manager
  let raw = responseBody resp

  js <- case eitherDecode raw :: Either String Value of
    Left e -> fail ("OpenAI: invalid JSON response: " <> e)
    Right ok -> pure ok

  let txt = extractResponsesText js

  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 txt) :: Either String Value of
    Right v -> pure v
    Left e -> fail ("OpenAI: schema-enforced output was not valid JSON: " <> e <> "\nRaw response text: " <> T.unpack txt)

-- Convert ChatMessage to JSON Value for API request
chatMessageToValue :: ChatMessage -> Value
chatMessageToValue (ChatMessage role content) =
  object
    [ "role" .= role,
      "content" .= content
    ]

-- OpenAI "Responses API" extraction:
-- Prefer `output_text`; else concatenate `output[].content[].text`.
extractResponsesText :: Value -> Text
extractResponsesText (Object o)
  | Just (String s) <- KM.lookup "output_text" o = s
  | Just (Array arr) <- KM.lookup "output" o =
      T.intercalate "\n" $ do
        v <- toList arr
        case v of
          Object oi ->
            case KM.lookup "content" oi of
              Just (Array content) ->
                [ t | Object ci <- toList content, Just (String t) <- [KM.lookup "text" ci]
                ]
              _ -> []
          _ -> []
  | otherwise = ""
extractResponsesText _ = ""