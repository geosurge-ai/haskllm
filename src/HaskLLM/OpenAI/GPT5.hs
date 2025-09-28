{-# LANGUAGE OverloadedStrings #-}

-- | OpenAI GPT-5 client using the Responses API, with structured-output enforcement.
--   Exposes a generic typeclass for conversational generation with (optional) JSON schema.
--   The vLLM/Qwen module imports this to implement the same interface against a different endpoint.
module HaskLLM.OpenAI.GPT5
  ( -- * Shared interface & types
    Credentials (..),
    ChatMessage (..),
    JSONSchemaSpec (..),
    LLMFormatChat (..),

    -- * Provider tag for OpenAI
    OpenAI (..),
  )
where

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
  )
import Network.HTTP.Client
  ( RequestBody (..),
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Provider tag for OpenAI GPT‑5 (Responses API).
data OpenAI = OpenAI

instance LLMFormatChat OpenAI where
  -- Responses API text output (no schema).
  respondText _ (Credentials cred) modelName msgs = liftIO $ do
    apiKey <- required "openai_api_key" cred
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest "https://api.openai.com/v1/responses"

    -- Convert ChatMessage to proper format for Responses API
    let inputMessages = map chatMessageToValue msgs
        body =
          object
            [ "model" .= modelName,
              "input" .= inputMessages,
              "max_output_tokens" .= (8192 :: Int)
            ]
        req =
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (900 * 1000000) -- 15 minutes for GPT-5 reasoning
            }

    resp <- httpLbs req manager
    let raw = responseBody resp

    case eitherDecode raw :: Either String Value of
      Left e -> fail ("OpenAI: invalid JSON response: " <> e)
      Right js -> pure (extractResponsesText js)

  -- Responses API structured output with JSON schema.
  respondJSON _ (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) = liftIO $ do
    apiKey <- required "openai_api_key" cred
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest "https://api.openai.com/v1/responses"

    -- Convert ChatMessage to proper format for Responses API
    let inputMessages = map chatMessageToValue msgs

    -- Based on API error: Responses API uses text.format, not response_format
    let textFormat =
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
              "max_output_tokens" .= (8192 :: Int)
            ]

        req =
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (900 * 1000000) -- 15 minutes for GPT-5 reasoning  -- 5 minutes for GPT-5 reasoning
            }

    resp <- httpLbs req manager
    let raw = responseBody resp

    js <- case eitherDecode raw :: Either String Value of
      Left e -> fail ("OpenAI: invalid JSON response: " <> e)
      Right ok -> pure ok

    -- Extract the model's textual payload (which should be pure JSON by schema),
    -- then parse it as JSON and return it.
    let txt = extractResponsesText js

    case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 txt) :: Either String Value of
      Right v -> pure v
      Left e -> fail ("OpenAI: schema-enforced output was not valid JSON: " <> e <> "\nRaw response text: " <> T.unpack txt)

  -- Responses API text output with configurable max tokens.
  respondTextWithTokens _ (Credentials cred) modelName msgs mMaxTokens = liftIO $ do
    apiKey <- required "openai_api_key" cred
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest "https://api.openai.com/v1/responses"

    -- Convert ChatMessage to proper format for Responses API
    let inputMessages = map chatMessageToValue msgs
        maxTokens = fromMaybe 8192 mMaxTokens
        body =
          object
            [ "model" .= modelName,
              "input" .= inputMessages,
              "max_output_tokens" .= maxTokens
            ]
        req =
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (900 * 1000000) -- 15 minutes for GPT-5 reasoning
            }

    resp <- httpLbs req manager
    let raw = responseBody resp

    case eitherDecode raw :: Either String Value of
      Left e -> fail ("OpenAI: invalid JSON response: " <> e)
      Right js -> pure (extractResponsesText js)

  -- Responses API structured output with JSON schema and configurable max tokens.
  respondJSONWithTokens _ (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) mMaxTokens = liftIO $ do
    apiKey <- required "openai_api_key" cred
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest "https://api.openai.com/v1/responses"

    -- Convert ChatMessage to proper format for Responses API
    let inputMessages = map chatMessageToValue msgs
        maxTokens = fromMaybe 8192 mMaxTokens

    -- Based on API error: Responses API uses text.format, not response_format
    let textFormat =
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
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (900 * 1000000) -- 15 minutes for GPT-5 reasoning
            }

    resp <- httpLbs req manager
    let raw = responseBody resp

    js <- case eitherDecode raw :: Either String Value of
      Left e -> fail ("OpenAI: invalid JSON response: " <> e)
      Right ok -> pure ok

    -- Extract the model's textual payload (which should be pure JSON by schema),
    -- then parse it as JSON and return it.
    let txt = extractResponsesText js

    case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 txt) :: Either String Value of
      Right v -> pure v
      Left e -> fail ("OpenAI: schema-enforced output was not valid JSON: " <> e <> "\nRaw response text: " <> T.unpack txt)

--------------------------------------------------------------------------------
-- Helpers

required :: Text -> Map Text Text -> IO Text
required k m = case M.lookup k m of
  Just v -> pure v
  Nothing -> fail ("Missing credential key: " <> T.unpack k)

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