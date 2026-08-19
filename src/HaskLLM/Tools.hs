-- | Native tool calling ("function calling") for providers that support it.
--
--   A 'Tool' pairs a 'ToolSpec' (name, description, JSON Schema for the
--   arguments) with a handler @a -> IO Text@. The argument type is
--   existentially quantified, so a heterogeneous list of tools can be handed
--   to the model:
--
-- @
--   data WeatherArgs = WeatherArgs { city :: Text }
--
--   instance FromJSON WeatherArgs where
--     parseJSON = withObject "WeatherArgs" $ \\o -> WeatherArgs \<$\> o .: "city"
--
--   weather :: Tool
--   weather =
--     Tool
--       (ToolSpec "get_weather" "Current weather for a city" weatherSchema True)
--       (\\(WeatherArgs c) -> pure ("Sunny in " <> c))
--
--   answer <- respondTools OpenAI creds "gpt-5" msgs [weather]
-- @
--
--   Providers implement 'LLMToolChat' by running a request\/execute\/feed-back
--   loop: the model's tool calls are parsed into the handler's argument type,
--   executed, and their outputs sent back until the model produces a final
--   text answer. Unknown tools and unparseable arguments are reported back to
--   the model as tool output so it can self-correct.
module HaskLLM.Tools (
  ToolSpec (..),
  Tool (..),
  ToolInvocation (..),
  ToolChatResult (..),
  LLMToolChat (..),
  respondTools,
  defaultMaxToolRounds,
  aggregateUsage,
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, Value)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import HaskLLM (
  ChatMessage,
  Credentials,
  LLMResponse (..),
  RequestConfig,
  TokenUsage (..),
  defaultRequestConfig,
 )

-- | Specification of a callable tool, parameterized by its argument type.
--   The type parameter is phantom here; it ties the spec to its handler in
--   'Tool'.
data ToolSpec a = ToolSpec
  { toolName :: Text,
    toolDescription :: Text,
    -- | JSON Schema for the tool's arguments (subset supported by providers).
    toolSchema :: Value,
    -- | Enforce exact schema conformance when supported.
    toolStrict :: Bool
  }
  deriving (Show, Eq)

-- | A tool the model may call: a spec plus a handler for the parsed arguments.
data Tool where
  Tool :: (FromJSON a) => ToolSpec a -> (a -> IO Text) -> Tool

-- | One tool call made by the model during a conversation.
data ToolInvocation = ToolInvocation
  { invokedName :: Text,
    -- | Raw JSON arguments as sent by the model.
    invokedArguments :: Text,
    -- | Output fed back to the model (handler result or dispatch error).
    invokedOutput :: Text
  }
  deriving (Show, Eq)

-- | Final assistant text plus a trace of every tool call made along the way.
data ToolChatResult = ToolChatResult
  { finalText :: Text,
    toolTrace :: [ToolInvocation]
  }
  deriving (Show, Eq)

-- | Providers that support native tool calling.
class LLMToolChat provider where
  -- | Chat with tools: runs the request\/execute\/feed-back loop until the
  --   model produces a final text answer, with normalized response metadata.
  --   Token usage is aggregated across all rounds of the loop.
  respondToolsDetailed ::
    (MonadIO m) =>
    provider ->
    Credentials ->
    -- | Model name
    Text ->
    [ChatMessage] ->
    [Tool] ->
    -- | Max output tokens per round
    Maybe Int ->
    -- | Max tool rounds before giving up
    Int ->
    RequestConfig ->
    m (LLMResponse ToolChatResult)

-- | Default cap on request\/execute rounds in the tool loop.
defaultMaxToolRounds :: Int
defaultMaxToolRounds = 10

-- | Chat with tools using defaults; returns just the final assistant text.
respondTools ::
  (LLMToolChat provider, MonadIO m) =>
  provider ->
  Credentials ->
  -- | Model name
  Text ->
  [ChatMessage] ->
  [Tool] ->
  m Text
respondTools prov creds model msgs tools =
  finalText . responseContent
    <$> respondToolsDetailed prov creds model msgs tools Nothing defaultMaxToolRounds defaultRequestConfig

-- | Sum token usage across multiple provider calls (e.g. tool-loop rounds).
--   Each field is summed over the rounds that reported it; a field is Nothing
--   only if no round reported it.
aggregateUsage :: [TokenUsage] -> Maybe TokenUsage
aggregateUsage [] = Nothing
aggregateUsage us =
  Just
    TokenUsage
      { inputTokens = total inputTokens,
        outputTokens = total outputTokens,
        totalTokens = total totalTokens,
        cachedInputTokens = total cachedInputTokens,
        reasoningTokens = total reasoningTokens
      }
 where
  total field = case mapMaybe field us of
    [] -> Nothing
    xs -> Just (sum xs)
