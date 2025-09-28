{-# LANGUAGE OverloadedStrings #-}

-- | vLLM (Qwen) client via OpenAI-compatible Chat Completions API.
--   Supports strict JSON schema enforcement using `response_format.json_schema`.
module HaskLLM.VLLM.QweN2_5
  ( Qwen (..),
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
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HaskLLM
  ( Credentials (..),
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

-- | Provider tag for vLLM/Qwen (OpenAI-compatible server).
data Qwen = Qwen

instance LLMFormatChat Qwen where
  -- Plain chat (no schema).
  respondText _ (Credentials cred) modelName msgs = liftIO $ do
    base <- required "base_url" cred
    apiKey <- required "api_key" cred
    session <- required "session_token" cred

    let url = concretizeChatEndpoint base
        body =
          object
            [ "model" .= modelName,
              "messages" .= msgs,
              "temperature" .= (0.7 :: Double)
            ]
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest (T.unpack url)
    let req =
          req0
            { method = "POST",
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  ("x-api-key", TE.encodeUtf8 apiKey),
                  ("x-session-token", TE.encodeUtf8 session)
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (120 * 1000000)
            }
    resp <- httpLbs req manager
    let raw = responseBody resp
    case eitherDecode raw :: Either String Value of
      Left e -> fail ("vLLM: invalid JSON response: " <> e)
      Right js -> pure (extractChatContent js)

  -- Enforced JSON schema via `response_format` (vLLM / chat.completions).
  respondJSON _ (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) = liftIO $ do
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
        body =
          object
            [ "model" .= modelName,
              "messages" .= msgs,
              "temperature" .= (0.7 :: Double),
              "response_format" .= responseFormat
            ]
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest (T.unpack url)
    let req =
          req0
            { method = "POST",
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  ("x-api-key", TE.encodeUtf8 apiKey),
                  ("x-session-token", TE.encodeUtf8 session)
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (120 * 1000000)
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

  -- Plain chat with configurable max tokens.
  respondTextWithTokens _ (Credentials cred) modelName msgs mMaxTokens = liftIO $ do
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
          req0
            { method = "POST",
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  ("x-api-key", TE.encodeUtf8 apiKey),
                  ("x-session-token", TE.encodeUtf8 session)
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (120 * 1000000)
            }
    resp <- httpLbs req manager
    let raw = responseBody resp
    case eitherDecode raw :: Either String Value of
      Left e -> fail ("vLLM: invalid JSON response: " <> e)
      Right js -> pure (extractChatContent js)

  -- Enforced JSON schema with configurable max tokens via `response_format` (vLLM / chat.completions).
  respondJSONWithTokens _ (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) mMaxTokens = liftIO $ do
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
          req0
            { method = "POST",
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  ("x-api-key", TE.encodeUtf8 apiKey),
                  ("x-session-token", TE.encodeUtf8 session)
                ],
              requestBody = RequestBodyLBS (encode body),
              responseTimeout = responseTimeoutMicro (120 * 1000000)
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

--------------------------------------------------------------------------------
-- Helpers (local)

required :: Text -> M.Map Text Text -> IO Text
required k m = case M.lookup k m of
  Just v -> pure v
  Nothing -> fail ("Missing credential key: " <> T.unpack k)

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
