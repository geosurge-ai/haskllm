{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskLLM (
  Credentials (..),
  ChatMessage (..),
  JSONSchemaSpec (..),
  RequestConfig (..),
  TokenUsage (..),
  AttemptObservation (..),
  AttemptOutcome (..),
  AttemptObserver,
  LLMResponse (..),
  defaultRequestConfig,
  LLMFormatChat (..),
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Simple credential bag.
--   Required keys:
--     - OpenAI GPT‑5:      "openai_api_key"
--     - vLLM / Qwen:       "base_url", "api_key"
--     - OpenRouter:        "openrouter_api_key"
newtype Credentials = Credentials (Map Text Text)
  deriving (Show, Generic)

-- | Configuration for HTTP requests
data RequestConfig = RequestConfig
  { -- | Timeout in seconds. Nothing means no timeout.
    timeoutSeconds :: Maybe Int,
    -- | Number of retries on failure (default: 3)
    maxRetries :: Int
  }
  deriving (Show, Eq, Generic)

-- | Default request configuration: no timeout, 3 retries
defaultRequestConfig :: RequestConfig
defaultRequestConfig =
  RequestConfig
    { timeoutSeconds = Nothing,
      maxRetries = 3
    }

-- | Minimal chat message (OpenAI / vLLM compatible).
data ChatMessage = ChatMessage
  { -- | "system" | "user" | "assistant"
    role :: Text,
    content :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatMessage where
  toJSON (ChatMessage r c) = object ["role" .= r, "content" .= c]

-- | JSON Schema spec (portable across providers).
--   For OpenAI Responses API: becomes @text.format@ payload.
--   For vLLM Chat Completions: becomes @response_format.json_schema@.
data JSONSchemaSpec = JSONSchemaSpec
  { schemaName :: Text,
    -- | A JSON Schema (subset supported by providers)
    schema :: Value,
    -- | Enforce exact conformance when supported
    strict :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Normalized provider-reported token usage.
--
-- Providers do not always return all fields, so every field is optional.
-- No pricing is computed here: 'costUsd' is only what the provider itself billed.
data TokenUsage = TokenUsage
  { -- | All input tokens, including cache reads and writes.
    inputTokens :: Maybe Int,
    -- | All output tokens, including reasoning tokens.
    outputTokens :: Maybe Int,
    -- | Provider-reported input plus output total.
    totalTokens :: Maybe Int,
    -- | Input tokens read from the cache; already included in 'inputTokens'.
    cachedInputTokens :: Maybe Int,
    -- | Input tokens written to the cache; already included in 'inputTokens'.
    -- Nothing means unreported, not zero. Cache writes can have their own rate.
    cacheWriteTokens :: Maybe Int,
    -- | Reasoning output, already included in 'outputTokens'.
    reasoningTokens :: Maybe Int,
    costUsd :: Maybe Double
  }
  deriving (Show, Eq, Generic)

-- | The transport outcome, not whether the generated content was usable.
data AttemptOutcome
  = HTTPResponse Int
  | TransportFailure Text
  deriving (Show, Eq, Generic)

-- | One HTTP attempt, observed before status handling or content parsing.
-- These records overlap with 'responseUsage'; don't charge for both.
data AttemptObservation = AttemptObservation
  { -- | Provider responsible for this attempt, including in a fallback chain.
    attemptProvider :: Text,
    -- | Returned model identifier, or the requested model if unavailable.
    attemptModel :: Text,
    -- | Provider request identifier from the HTTP headers, when received.
    attemptRequestId :: Maybe Text,
    -- | Provider response identifier, when the envelope could be decoded.
    attemptResponseId :: Maybe Text,
    -- | Provider status (e.g. completed or incomplete), distinct from HTTP status.
    attemptResponseStatus :: Maybe Text,
    -- | HTTP status or transport failure; neither establishes billability.
    attemptOutcome :: AttemptOutcome,
    -- | Usage reported for this attempt. Missing usage is unknown, not free.
    attemptUsage :: Maybe TokenUsage
  }
  deriving (Show, Eq, Generic)

-- | Synchronous, best-effort accounting hook. Synchronous exceptions are logged
-- and ignored; cancellation propagates. Keep callbacks short. No delivery is
-- guaranteed after cancellation or process termination.
type AttemptObserver = AttemptObservation -> IO ()

-- | A generated response plus normalized metadata from the provider call.
data LLMResponse a = LLMResponse
  { responseContent :: a,
    responseUsage :: Maybe TokenUsage,
    responseModel :: Text,
    responseProvider :: Text
  }
  deriving (Show, Eq, Generic)

-- | Generic interface for conversational generation with optional JSON format enforcement.
class LLMFormatChat provider where
  -- | Plain chat; returns assistant text.
  respondText ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    m Text

  -- | Chat with enforced JSON schema; returns parsed JSON (throws on invalid JSON).
  respondJSON ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    JSONSchemaSpec ->
    m Value

  -- | Plain chat with configurable max tokens; returns assistant text.
  respondTextWithTokens ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    Maybe Int ->
    m Text

  -- | Chat with enforced JSON schema and configurable max tokens; returns parsed JSON (throws on invalid JSON).
  respondJSONWithTokens ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    JSONSchemaSpec ->
    Maybe Int ->
    m Value

  -- | Plain chat with configurable timeout and retries; returns assistant text.
  respondTextWithConfig ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    RequestConfig ->
    m Text

  -- | Chat with enforced JSON schema and configurable timeout and retries; returns parsed JSON.
  respondJSONWithConfig ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    JSONSchemaSpec ->
    RequestConfig ->
    m Value

  -- | Plain chat with configurable max tokens, timeout and retries; returns assistant text.
  respondTextWithTokensAndConfig ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    Maybe Int ->
    RequestConfig ->
    m Text

  -- | Chat with enforced JSON schema, configurable max tokens, timeout and retries; returns parsed JSON.
  respondJSONWithTokensAndConfig ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    JSONSchemaSpec ->
    Maybe Int ->
    RequestConfig ->
    m Value

  -- | Plain chat with normalized response metadata.
  respondTextDetailed ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    Maybe Int ->
    RequestConfig ->
    m (LLMResponse Text)

  -- | JSON chat with normalized response metadata.
  respondJSONDetailed ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    Text ->
    [ChatMessage] ->
    JSONSchemaSpec ->
    Maybe Int ->
    RequestConfig ->
    m (LLMResponse Value)

  -- Default implementations for backwards compatibility
  respondTextWithTokens prov creds model msgs _ = respondText prov creds model msgs
  respondJSONWithTokens prov creds model msgs schema _ = respondJSON prov creds model msgs schema

  -- Default implementations using defaultRequestConfig
  respondTextWithConfig prov creds model msgs _ = respondText prov creds model msgs
  respondJSONWithConfig prov creds model msgs schema _ = respondJSON prov creds model msgs schema
  respondTextWithTokensAndConfig prov creds model msgs tokens _ = respondTextWithTokens prov creds model msgs tokens
  respondJSONWithTokensAndConfig prov creds model msgs schema tokens _ = respondJSONWithTokens prov creds model msgs schema tokens
  respondTextDetailed prov creds model msgs tokens config = do
    txt <- respondTextWithTokensAndConfig prov creds model msgs tokens config
    pure $
      LLMResponse
        { responseContent = txt,
          responseUsage = Nothing,
          responseModel = model,
          responseProvider = "unknown"
        }
  respondJSONDetailed prov creds model msgs schema tokens config = do
    val <- respondJSONWithTokensAndConfig prov creds model msgs schema tokens config
    pure $
      LLMResponse
        { responseContent = val,
          responseUsage = Nothing,
          responseModel = model,
          responseProvider = "unknown"
        }
