{-# LANGUAGE OverloadedStrings #-}

-- | Phony LLM provider that always fails.
--   Useful for testing fallback mechanisms and error handling.
--
-- Example usage:
-- @
--   -- This will always throw an exception
--   result <- respondText Phony undefined "phony-model" messages
--
--   -- But in a fallback chain, it will skip to the next provider
--   let fallback = FallbackProvider
--         { primary = ProviderConfig Phony undefined "phony"
--         , secondary = ProviderConfig Qwen qwenCreds "Qwen/Qwen2.5-32B-Instruct"
--         }
--   result <- respondText fallback undefined undefined messages  -- Uses Qwen
-- @
module HaskLLM.Phony
  ( Phony (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import HaskLLM (LLMFormatChat (..))

-- | Provider tag for a phony/mock LLM that always fails.
--   Throws an error on every method call.
data Phony = Phony
  deriving (Show, Eq)

-- | Helper that always fails with a descriptive error
phonyFail :: (MonadIO m) => String -> m a
phonyFail method = liftIO $ fail $ "Phony LLM provider: " <> method <> " intentionally failed"

instance LLMFormatChat Phony where
  respondText _ _ _ _ = phonyFail "respondText"
  respondJSON _ _ _ _ _ = phonyFail "respondJSON"
  respondTextWithTokens _ _ _ _ _ = phonyFail "respondTextWithTokens"
  respondJSONWithTokens _ _ _ _ _ _ = phonyFail "respondJSONWithTokens"
  respondTextWithConfig _ _ _ _ _ = phonyFail "respondTextWithConfig"
  respondJSONWithConfig _ _ _ _ _ _ = phonyFail "respondJSONWithConfig"
  respondTextWithTokensAndConfig _ _ _ _ _ _ = phonyFail "respondTextWithTokensAndConfig"
  respondJSONWithTokensAndConfig _ _ _ _ _ _ _ = phonyFail "respondJSONWithTokensAndConfig"
