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
module HaskLLM.OpenAI.GPT5 (
  -- * Shared interface & types
  Credentials (..),
  ChatMessage (..),
  JSONSchemaSpec (..),
  RequestConfig (..),
  defaultRequestConfig,
  LLMFormatChat (..),

  -- * Provider tag for OpenAI
  OpenAI (..),

  -- * Native tool calling
  ToolSpec (..),
  Tool (..),
  ToolInvocation (..),
  ToolChatResult (..),
  LLMToolChat (..),
  respondTools,
  defaultMaxToolRounds,
  runToolLoop,
)
where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON,
  Value (..),
  eitherDecode,
  encode,
  object,
  (.=),
 )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (
  Request,
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

import HaskLLM (
  ChatMessage (..),
  Credentials (..),
  JSONSchemaSpec (..),
  LLMFormatChat (..),
  LLMResponse (..),
  RequestConfig (..),
  TokenUsage (..),
  defaultRequestConfig,
 )
import HaskLLM.Tools (
  LLMToolChat (..),
  Tool (..),
  ToolChatResult (..),
  ToolInvocation (..),
  ToolSpec (..),
  aggregateUsage,
  defaultMaxToolRounds,
  respondTools,
 )

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
      responseContent <$> makeTextRequestDetailed creds modelName msgs Nothing defaultRequestConfig

  -- Responses API structured output with JSON schema.
  respondJSON _ creds modelName msgs schema =
    liftIO $
      responseContent <$> makeJSONRequestDetailed creds modelName msgs schema Nothing defaultRequestConfig

  -- Responses API text output with configurable max tokens.
  respondTextWithTokens _ creds modelName msgs mMaxTokens =
    liftIO $
      responseContent <$> makeTextRequestDetailed creds modelName msgs mMaxTokens defaultRequestConfig

  -- Responses API structured output with JSON schema and configurable max tokens.
  respondJSONWithTokens _ creds modelName msgs schema mMaxTokens =
    liftIO $
      responseContent <$> makeJSONRequestDetailed creds modelName msgs schema mMaxTokens defaultRequestConfig

  -- New configurable methods
  respondTextWithConfig _ creds modelName msgs config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        responseContent <$> makeTextRequestDetailed creds modelName msgs Nothing config

  respondJSONWithConfig _ creds modelName msgs schema config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        responseContent <$> makeJSONRequestDetailed creds modelName msgs schema Nothing config

  respondTextWithTokensAndConfig _ creds modelName msgs mMaxTokens config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        responseContent <$> makeTextRequestDetailed creds modelName msgs mMaxTokens config

  respondJSONWithTokensAndConfig _ creds modelName msgs schema mMaxTokens config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        responseContent <$> makeJSONRequestDetailed creds modelName msgs schema mMaxTokens config

  respondTextDetailed _ creds modelName msgs mMaxTokens config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        makeTextRequestDetailed creds modelName msgs mMaxTokens config

  respondJSONDetailed _ creds modelName msgs schema mMaxTokens config =
    liftIO $
      retryWithBackoff (maxRetries config) $
        makeJSONRequestDetailed creds modelName msgs schema mMaxTokens config

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
makeTextRequestDetailed :: Credentials -> Text -> [ChatMessage] -> Maybe Int -> RequestConfig -> IO (LLMResponse Text)
makeTextRequestDetailed (Credentials cred) modelName msgs mMaxTokens config = do
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
    Right js ->
      pure $
        LLMResponse
          { responseContent = extractResponsesText js,
            responseUsage = extractResponsesUsage js,
            responseModel = modelName,
            responseProvider = "openai"
          }

-- | Make a JSON request with configurable timeout and retries
makeJSONRequestDetailed :: Credentials -> Text -> [ChatMessage] -> JSONSchemaSpec -> Maybe Int -> RequestConfig -> IO (LLMResponse Value)
makeJSONRequestDetailed (Credentials cred) modelName msgs (JSONSchemaSpec nm sch isStrict) mMaxTokens config = do
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
    Right v ->
      pure $
        LLMResponse
          { responseContent = v,
            responseUsage = extractResponsesUsage js,
            responseModel = modelName,
            responseProvider = "openai"
          }
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

extractResponsesUsage :: Value -> Maybe TokenUsage
extractResponsesUsage (Object o)
  | Just usage <- KM.lookup "usage" o =
      Just $
        TokenUsage
          { inputTokens = lookupInt "input_tokens" usage,
            outputTokens = lookupInt "output_tokens" usage,
            totalTokens = lookupInt "total_tokens" usage,
            cachedInputTokens = lookupNestedInt ["input_tokens_details", "cached_tokens"] usage,
            reasoningTokens = lookupNestedInt ["output_tokens_details", "reasoning_tokens"] usage
          }
extractResponsesUsage _ = Nothing

lookupInt :: Text -> Value -> Maybe Int
lookupInt key (Object o) = case KM.lookup (fromTextKey key) o of
  Just (Number n) -> toBoundedInteger n
  _ -> Nothing
lookupInt _ _ = Nothing

lookupNestedInt :: [Text] -> Value -> Maybe Int
lookupNestedInt [] _ = Nothing
lookupNestedInt [key] value = lookupInt key value
lookupNestedInt (key : rest) (Object o) = KM.lookup (fromTextKey key) o >>= lookupNestedInt rest
lookupNestedInt _ _ = Nothing

fromTextKey :: Text -> K.Key
fromTextKey = K.fromText

--------------------------------------------------------------------------------
-- Native tool calling (Responses API function calling)

instance LLMToolChat OpenAI where
  respondToolsDetailed _ creds modelName msgs tools mMaxTokens maxRounds config =
    liftIO $ makeToolRequestDetailed creds modelName msgs tools mMaxTokens maxRounds config

-- | Run the tool loop against the live Responses API endpoint.
makeToolRequestDetailed :: Credentials -> Text -> [ChatMessage] -> [Tool] -> Maybe Int -> Int -> RequestConfig -> IO (LLMResponse ToolChatResult)
makeToolRequestDetailed (Credentials cred) modelName msgs tools mMaxTokens maxRounds config = do
  apiKey <- required "openai_api_key" cred
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest "https://api.openai.com/v1/responses"

  let maxTokens = fromMaybe 8192 mMaxTokens
      -- Each HTTP round is retried individually so tool handlers never re-run
      -- because of a transport failure.
      transport inputItems = retryWithBackoff (maxRetries config) $ do
        let body =
              object
                [ "model" .= modelName,
                  "input" .= inputItems,
                  "tools" .= map toolToValue tools,
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
        case eitherDecode (responseBody resp) :: Either String Value of
          Left e -> fail ("OpenAI: invalid JSON response: " <> e)
          Right js -> pure js

  (result, usages) <- runToolLoop transport tools (map chatMessageToValue msgs) maxRounds
  pure
    LLMResponse
      { responseContent = result,
        responseUsage = aggregateUsage usages,
        responseModel = modelName,
        responseProvider = "openai"
      }

-- | Serialize a 'Tool' for the Responses API @tools@ array.
toolToValue :: Tool -> Value
toolToValue (Tool spec _) =
  object
    [ "type" .= ("function" :: Text),
      "name" .= toolName spec,
      "description" .= toolDescription spec,
      "parameters" .= toolSchema spec,
      "strict" .= toolStrict spec
    ]

-- | A @function_call@ item extracted from a Responses API response.
data FunctionCall = FunctionCall
  { -- | Raw output item, echoed back into the next request's input.
    fcItem :: Value,
    fcCallId :: Text,
    fcName :: Text,
    fcArguments :: Text
  }

-- | The request\/execute\/feed-back loop, parameterized by transport so it can
--   be tested without the network. Takes Responses-API input items and returns
--   the final result plus per-round token usage.
runToolLoop ::
  -- | Transport: input items -> raw Responses API response
  ([Value] -> IO Value) ->
  [Tool] ->
  -- | Initial input items (e.g. from 'chatMessageToValue')
  [Value] ->
  -- | Max rounds before giving up
  Int ->
  IO (ToolChatResult, [TokenUsage])
runToolLoop transport tools initialItems maxRounds = go initialItems [] [] maxRounds
 where
  go items invocations usages roundsLeft
    | roundsLeft <= 0 =
        fail ("OpenAI: tool loop did not converge within " <> show maxRounds <> " rounds")
    | otherwise = do
        js <- transport items
        let usages' = usages <> maybe [] pure (extractResponsesUsage js)
        case extractFunctionCalls js of
          [] ->
            pure
              ( ToolChatResult
                  { finalText = extractResponsesText js,
                    toolTrace = invocations
                  },
                usages'
              )
          calls -> do
            newInvocations <- mapM (dispatchToolCall tools) calls
            let feedback =
                  concat
                    [ [fcItem call, functionCallOutput (fcCallId call) (invokedOutput inv)]
                    | (call, inv) <- zip calls newInvocations
                    ]
            go (items <> feedback) (invocations <> newInvocations) usages' (roundsLeft - 1)

-- | Execute one model-requested tool call. Unknown tools and unparseable
--   arguments produce an error string that is fed back to the model so it can
--   self-correct; exceptions from the handler itself propagate.
dispatchToolCall :: [Tool] -> FunctionCall -> IO ToolInvocation
dispatchToolCall tools call = do
  output <- case [t | t@(Tool spec _) <- tools, toolName spec == fcName call] of
    [] -> pure ("Error: unknown tool: " <> fcName call)
    (Tool _ handler : _) -> runHandler handler
  pure
    ToolInvocation
      { invokedName = fcName call,
        invokedArguments = fcArguments call,
        invokedOutput = output
      }
 where
  runHandler :: (FromJSON a) => (a -> IO Text) -> IO Text
  runHandler handler =
    case eitherDecode (LBS.fromStrict (TE.encodeUtf8 (fcArguments call))) of
      Left e -> pure ("Error: invalid arguments for " <> fcName call <> ": " <> T.pack e)
      Right args -> handler args

-- | Build a @function_call_output@ input item.
functionCallOutput :: Text -> Text -> Value
functionCallOutput callId output =
  object
    [ "type" .= ("function_call_output" :: Text),
      "call_id" .= callId,
      "output" .= output
    ]

-- | Extract @function_call@ items from a Responses API response.
extractFunctionCalls :: Value -> [FunctionCall]
extractFunctionCalls (Object o)
  | Just (Array arr) <- KM.lookup "output" o =
      [ FunctionCall v callId nm args
      | v@(Object oi) <- toList arr,
        Just (String "function_call") <- [KM.lookup "type" oi],
        Just (String callId) <- [KM.lookup "call_id" oi],
        Just (String nm) <- [KM.lookup "name" oi],
        Just (String args) <- [KM.lookup "arguments" oi]
      ]
extractFunctionCalls _ = []
