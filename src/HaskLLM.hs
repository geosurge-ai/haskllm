{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskLLM
  ( Credentials (..),
    ChatMessage (..),
    JSONSchemaSpec (..),
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
--     - vLLM / Qwen:       "base_url", "api_key", "session_token"
newtype Credentials = Credentials (Map Text Text)
  deriving (Show, Generic)

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
