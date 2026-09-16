{-# LANGUAGE OverloadedStrings #-}

-- | OpenRouter client via the OpenAI-compatible Chat Completions API.
--   Supports strict JSON schema enforcement using `response_format.json_schema`.
--
--   Credentials can be provided via the Credentials map or environment variables:
--   - @openrouter_api_key@: OPENROUTER_API_KEY (required)
module HaskLLM.OpenRouter (
  OpenRouter (..),
  chatCompletionsBody,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (
  RequestBody (..),
  httpLbs,
  method,
  parseRequest,
  requestBody,
  requestHeaders,
  responseBody,
 )
import Network.HTTP.Client.TLS (getGlobalManager)

import HaskLLM (
  ChatMessage (..),
  Credentials (..),
  JSONSchemaSpec (..),
  LLMFormatChat (..),
  LLMResponse (..),
  RequestConfig (..),
  defaultRequestConfig,
 )
import HaskLLM.Internal (
  configureTimeout,
  decodeSchemaOutput,
  extractChatContent,
  extractChatUsage,
  required,
 )
import HaskLLM.OpenAI.Retry (checkOpenAIResponse, retryOpenAIRequest)

-- | Provider tag.
--   The routing preferences are sent verbatim as the request's @provider@ field (host order, fallbacks, quantization, ...).
newtype OpenRouter = OpenRouter {openRouterRouting :: Maybe Value}

instance LLMFormatChat OpenRouter where
  respondText p creds modelName msgs =
    liftIO $ responseContent <$> request p creds modelName msgs Nothing Nothing defaultRequestConfig
  respondJSON p creds modelName msgs schema =
    liftIO $ responseContent <$> jsonRequest p creds modelName msgs schema Nothing defaultRequestConfig
  respondTextWithTokens p creds modelName msgs mMaxTokens =
    liftIO $ responseContent <$> request p creds modelName msgs Nothing mMaxTokens defaultRequestConfig
  respondJSONWithTokens p creds modelName msgs schema mMaxTokens =
    liftIO $ responseContent <$> jsonRequest p creds modelName msgs schema mMaxTokens defaultRequestConfig
  respondTextWithConfig p creds modelName msgs config =
    liftIO $ responseContent <$> request p creds modelName msgs Nothing Nothing config
  respondJSONWithConfig p creds modelName msgs schema config =
    liftIO $ responseContent <$> jsonRequest p creds modelName msgs schema Nothing config
  respondTextWithTokensAndConfig p creds modelName msgs mMaxTokens config =
    liftIO $ responseContent <$> request p creds modelName msgs Nothing mMaxTokens config
  respondJSONWithTokensAndConfig p creds modelName msgs schema mMaxTokens config =
    liftIO $ responseContent <$> jsonRequest p creds modelName msgs schema mMaxTokens config
  respondTextDetailed p creds modelName msgs mMaxTokens config =
    liftIO $ request p creds modelName msgs Nothing mMaxTokens config
  respondJSONDetailed p creds modelName msgs schema mMaxTokens config =
    liftIO $ jsonRequest p creds modelName msgs schema mMaxTokens config

-- | The Chat Completions request body. Optional fields are omitted, not nulled.
chatCompletionsBody :: OpenRouter -> Text -> [ChatMessage] -> Maybe JSONSchemaSpec -> Maybe Int -> Value
chatCompletionsBody (OpenRouter routing) modelName msgs mSchema mMaxTokens =
  object $
    [ "model" .= modelName,
      "messages" .= msgs
    ]
      <> catMaybes
        [ ("provider" .=) <$> routing,
          ("response_format" .=) . responseFormat <$> mSchema,
          ("max_tokens" .=) <$> mMaxTokens
        ]
 where
  responseFormat (JSONSchemaSpec nm sch isStrict) =
    object
      [ "type" .= ("json_schema" :: Text),
        "json_schema" .= object ["name" .= nm, "schema" .= sch, "strict" .= isStrict]
      ]

jsonRequest :: OpenRouter -> Credentials -> Text -> [ChatMessage] -> JSONSchemaSpec -> Maybe Int -> RequestConfig -> IO (LLMResponse Value)
jsonRequest provider creds modelName msgs schema mMaxTokens config =
  request provider creds modelName msgs (Just schema) mMaxTokens config >>= decodeSchemaOutput

request :: OpenRouter -> Credentials -> Text -> [ChatMessage] -> Maybe JSONSchemaSpec -> Maybe Int -> RequestConfig -> IO (LLMResponse Text)
request provider (Credentials cred) modelName msgs mSchema mMaxTokens config = do
  apiKey <- required "openrouter_api_key" "OPENROUTER_API_KEY" cred
  manager <- getGlobalManager
  req0 <- parseRequest "https://openrouter.ai/api/v1/chat/completions"
  let req =
        configureTimeout config $
          req0
            { method = "POST",
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (encode (chatCompletionsBody provider modelName msgs mSchema mMaxTokens))
            }
  resp <-
    retryOpenAIRequest (maxRetries config) $
      httpLbs req manager >>= checkOpenAIResponse
  case eitherDecode (responseBody resp) of
    Left e -> fail ("OpenRouter: invalid JSON response: " <> e)
    Right js -> do
      content <- either (fail . ("OpenRouter: " <>)) pure (extractChatContent js)
      pure
        LLMResponse
          { responseContent = content,
            responseUsage = extractChatUsage js,
            responseModel = modelName,
            -- The upstream host OpenRouter routed the request to.
            responseProvider = case js of
              Object o | Just (String host) <- KM.lookup "provider" o -> "openrouter/" <> host
              _ -> "openrouter"
          }
