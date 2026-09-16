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

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON,
  Value (..),
  eitherDecode,
  encode,
  object,
  (.=),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (
  RequestBody (..),
  checkResponse,
  httpLbs,
  method,
  newManager,
  parseRequest,
  requestBody,
  requestHeaders,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)

import HaskLLM (
  AttemptObserver,
  ChatMessage (..),
  Credentials (..),
  JSONSchemaSpec (..),
  LLMFormatChat (..),
  LLMResponse (..),
  RequestConfig (..),
  TokenUsage (..),
  defaultRequestConfig,
 )
import HaskLLM.Internal (configureTimeout, required)
import HaskLLM.OpenAI.Request (
  extractResponsesText,
  extractResponsesUsage,
  parseJSONContent,
  requestOpenAI,
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

-- | Responses API provider. The observer follows the provider through Pandoc
-- calls and fallback chains, without changing their method signatures.
data OpenAI
  = OpenAI
  | OpenAIWithObserver AttemptObserver

instance LLMFormatChat OpenAI where
  respondText prov creds modelName msgs =
    responseContent <$> respondTextDetailed prov creds modelName msgs Nothing defaultRequestConfig
  respondJSON prov creds modelName msgs schema =
    responseContent <$> respondJSONDetailed prov creds modelName msgs schema Nothing defaultRequestConfig
  respondTextWithTokens prov creds modelName msgs tokens =
    responseContent <$> respondTextDetailed prov creds modelName msgs tokens defaultRequestConfig
  respondJSONWithTokens prov creds modelName msgs schema tokens =
    responseContent <$> respondJSONDetailed prov creds modelName msgs schema tokens defaultRequestConfig
  respondTextWithConfig prov creds modelName msgs config =
    responseContent <$> respondTextDetailed prov creds modelName msgs Nothing config
  respondJSONWithConfig prov creds modelName msgs schema config =
    responseContent <$> respondJSONDetailed prov creds modelName msgs schema Nothing config
  respondTextWithTokensAndConfig prov creds modelName msgs tokens config =
    responseContent <$> respondTextDetailed prov creds modelName msgs tokens config
  respondJSONWithTokensAndConfig prov creds modelName msgs schema tokens config =
    responseContent <$> respondJSONDetailed prov creds modelName msgs schema tokens config
  respondTextDetailed prov creds modelName msgs tokens config =
    liftIO $ makeChatRequestDetailed prov creds modelName msgs [] tokens config
  respondJSONDetailed prov creds modelName msgs (JSONSchemaSpec name schema strict) tokens config =
    liftIO do
      response <-
        makeChatRequestDetailed
          prov
          creds
          modelName
          msgs
          [ "text"
              .= object
                [ "format"
                    .= object
                      [ "type" .= ("json_schema" :: Text),
                        "name" .= name,
                        "schema" .= schema,
                        "strict" .= strict
                      ]
                ]
          ]
          tokens
          config
      content <- parseJSONContent $ responseContent response
      pure response {responseContent = content}

--------------------------------------------------------------------------------
-- Helpers

-- | Share request construction and observation across text, JSON and tool calls.
-- Each tool round uses the same manager and retries only its own HTTP attempt.
makeTransport :: OpenAI -> Credentials -> Text -> [Pair] -> Maybe Int -> RequestConfig -> IO ([Value] -> IO Value)
makeTransport prov (Credentials cred) modelName extra mMaxTokens config = do
  apiKey <- required "openai_api_key" "OPENAI_API_KEY" cred
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest "https://api.openai.com/v1/responses"
  let observe = case prov of
        OpenAI -> const $ pure ()
        OpenAIWithObserver observer -> observer
  pure \input ->
    requestOpenAI observe modelName (maxRetries config) $
      flip httpLbs manager $
        configureTimeout config $
          req0
            { method = "POST",
              -- Observe error responses before status handling can discard them.
              checkResponse = \_ _ -> pure (),
              requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                  ("Content-Type", "application/json")
                ],
              requestBody =
                RequestBodyLBS $
                  encode $
                    object $
                      [ "model" .= modelName,
                        "input" .= input,
                        "max_output_tokens" .= fromMaybe 8192 mMaxTokens
                      ]
                        <> extra
            }

makeChatRequestDetailed :: OpenAI -> Credentials -> Text -> [ChatMessage] -> [Pair] -> Maybe Int -> RequestConfig -> IO (LLMResponse Text)
makeChatRequestDetailed prov creds modelName msgs extra tokens config = do
  transport <- makeTransport prov creds modelName extra tokens config
  response <- transport $ map chatMessageToValue msgs
  pure
    LLMResponse
      { responseContent = extractResponsesText response,
        responseUsage = extractResponsesUsage response,
        responseModel = modelName,
        responseProvider = "openai"
      }

-- Convert ChatMessage to JSON Value for API request
chatMessageToValue :: ChatMessage -> Value
chatMessageToValue (ChatMessage role content) =
  object
    [ "role" .= role,
      "content" .= content
    ]

--------------------------------------------------------------------------------
-- Native tool calling (Responses API function calling)

instance LLMToolChat OpenAI where
  respondToolsDetailed prov creds modelName msgs tools mMaxTokens maxRounds config =
    liftIO $ makeToolRequestDetailed prov creds modelName msgs tools mMaxTokens maxRounds config

-- | Run the tool loop against the live Responses API endpoint.
makeToolRequestDetailed :: OpenAI -> Credentials -> Text -> [ChatMessage] -> [Tool] -> Maybe Int -> Int -> RequestConfig -> IO (LLMResponse ToolChatResult)
makeToolRequestDetailed prov creds modelName msgs tools mMaxTokens maxRounds config = do
  transport <- makeTransport prov creds modelName ["tools" .= map toolToValue tools] mMaxTokens config
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
  { fcCallId :: Text,
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
            -- Reasoning models require the complete prior output sequence
            -- (including reasoning items) when continuing a stateless tool
            -- turn, so echo every output item back before the tool outputs.
            let feedback =
                  extractOutputItems js
                    <> [ functionCallOutput (fcCallId call) (invokedOutput inv)
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

-- | Extract the raw @output@ items from a Responses API response.
extractOutputItems :: Value -> [Value]
extractOutputItems (Object o)
  | Just (Array arr) <- KM.lookup "output" o = toList arr
extractOutputItems _ = []

-- | Extract @function_call@ items from a Responses API response.
extractFunctionCalls :: Value -> [FunctionCall]
extractFunctionCalls js =
  [ FunctionCall callId nm args
  | Object oi <- extractOutputItems js,
    Just (String "function_call") <- [KM.lookup "type" oi],
    Just (String callId) <- [KM.lookup "call_id" oi],
    Just (String nm) <- [KM.lookup "name" oi],
    Just (String args) <- [KM.lookup "arguments" oi]
  ]
