{-# LANGUAGE OverloadedStrings #-}

-- | Plumbing shared by the provider clients.
module HaskLLM.Internal (
  configureTimeout,
  decodeSchemaOutput,
  extractChatContent,
  extractChatUsage,
  lookupInt,
  lookupNestedInt,
  required,
  stripCodeFence,
)
where

import Data.Aeson (Key, Value (..), eitherDecodeStrictText, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (
  Request,
  responseTimeout,
  responseTimeoutMicro,
  responseTimeoutNone,
 )
import System.Environment (lookupEnv)

import HaskLLM (LLMResponse (..), RequestConfig (..), TokenUsage (..))

-- | Configure timeout for a request based on RequestConfig
configureTimeout :: RequestConfig -> Request -> Request
configureTimeout config req = case timeoutSeconds config of
  Nothing -> req {responseTimeout = responseTimeoutNone}
  Just seconds -> req {responseTimeout = responseTimeoutMicro (seconds * 1000000)}

-- | A credential from the map, else from the named environment variable.
required :: Text -> String -> Map Text Text -> IO Text
required key envVar cred = maybe fromEnv pure (M.lookup key cred)
 where
  fromEnv = lookupEnv envVar >>= maybe missing (pure . T.pack)
  missing =
    fail $
      "Missing credential key: "
        <> T.unpack key
        <> " (not in Credentials map, and environment variable "
        <> envVar
        <> " is not set)"

-- | Assistant content of the first choice.
--   REFUSES error bodies and non-"stop" finishes, which OpenAI-compatible hosts may serve with HTTP 200.
extractChatContent :: Value -> Either String Text
extractChatContent (Object o)
  | Just err <- KM.lookup "error" o = Left ("upstream error: " <> json err)
  | Just (Array choices) <- KM.lookup "choices" o,
    (Object choice : _) <- toList choices =
      firstChoice choice
extractChatContent js = Left ("no choices in response: " <> json js)

firstChoice :: KM.KeyMap Value -> Either String Text
firstChoice choice
  | Just err <- KM.lookup "error" choice = Left ("upstream error: " <> json err)
  | Just (String reason) <- KM.lookup "finish_reason" choice,
    reason /= "stop" =
      Left ("finish_reason " <> T.unpack reason)
  | Just (Object msg) <- KM.lookup "message" choice,
    Just (String content) <- KM.lookup "content" msg =
      Right content
  | otherwise = Left ("no message content in choice: " <> json (Object choice))

json :: Value -> String
json = LBS8.unpack . encode

-- | Strip a ```json ... ``` or ``` ... ``` fence, single- or multi-line.
stripCodeFence :: Text -> Text
stripCodeFence t = case T.stripPrefix "```" stripped of
  Nothing -> stripped
  Just rest -> T.strip . T.dropWhile isAlpha $ fromMaybe rest (T.stripSuffix "```" rest)
 where
  stripped = T.strip t

-- | Parse schema-enforced output, failing with the provider name and raw text.
decodeSchemaOutput :: LLMResponse Text -> IO (LLMResponse Value)
decodeSchemaOutput response = case eitherDecodeStrictText (stripCodeFence raw) of
  Right v -> pure response {responseContent = v}
  Left e ->
    fail $
      T.unpack (responseProvider response)
        <> ": schema-enforced output was not valid JSON: "
        <> e
        <> "\nRaw response text: "
        <> T.unpack raw
 where
  raw = responseContent response

extractChatUsage :: Value -> Maybe TokenUsage
extractChatUsage (Object o)
  | Just usage <- KM.lookup "usage" o =
      Just $
        TokenUsage
          { inputTokens = lookupInt "prompt_tokens" usage,
            outputTokens = lookupInt "completion_tokens" usage,
            totalTokens = lookupInt "total_tokens" usage,
            cachedInputTokens = lookupNestedInt ["prompt_tokens_details", "cached_tokens"] usage,
            cacheWriteTokens = Nothing,
            reasoningTokens = lookupNestedInt ["completion_tokens_details", "reasoning_tokens"] usage,
            costUsd = toRealFloat <$> lookupNumber "cost" usage
          }
extractChatUsage _ = Nothing

lookupNumber :: Key -> Value -> Maybe Scientific
lookupNumber key (Object o) | Just (Number n) <- KM.lookup key o = Just n
lookupNumber _ _ = Nothing

lookupInt :: Key -> Value -> Maybe Int
lookupInt key value = toBoundedInteger =<< lookupNumber key value

lookupNestedInt :: [Key] -> Value -> Maybe Int
lookupNestedInt [] _ = Nothing
lookupNestedInt [key] value = lookupInt key value
lookupNestedInt (key : rest) (Object o) = KM.lookup key o >>= lookupNestedInt rest
lookupNestedInt _ _ = Nothing
