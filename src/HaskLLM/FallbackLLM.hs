{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Type-safe LLM provider fallback mechanism.
--   Attempts to use a primary provider, and falls back to a secondary provider on failure.
--
-- = Basic Usage (Two Providers)
--
-- @
--   import qualified Data.Map.Strict as M
--   import HaskLLM.FallbackLLM
--   import HaskLLM.VLLM.QweN2_5 (Qwen(..))
--   import HaskLLM.OpenAI.GPT5 (OpenAI(..))
--
--   let qwenCreds = Credentials $ M.fromList [("base_url", "https://..."), ("api_key", "..."), ("session_token", "...")]
--       gpt5Creds = Credentials $ M.fromList [("openai_api_key", "sk-...")]
--       fallback = FallbackProvider
--         { primary = ProviderConfig Qwen qwenCreds "Qwen/Qwen2.5-32B-Instruct"
--         , secondary = ProviderConfig OpenAI gpt5Creds "gpt-5-preview"
--         }
--   result <- respondText fallback undefined undefined messages
-- @
--
-- = Chaining Multiple Providers (Three or More)
--
-- Since 'FallbackProvider' itself implements 'LLMFormatChat', you can nest them
-- to create arbitrary fallback chains. Here's an ergonomic pattern for three providers:
--
-- @
--   -- Option 1: Explicit nesting (most transparent)
--   let threeWayFallback = FallbackProvider
--         { primary = ProviderConfig Qwen qwenCreds "Qwen/Qwen2.5-32B-Instruct"
--         , secondary = ProviderConfig
--             (FallbackProvider
--               { primary = ProviderConfig Claude claudeCreds "claude-3-opus"
--               , secondary = ProviderConfig OpenAI gpt5Creds "gpt-5-preview"
--               })
--             undefined  -- credentials unused for FallbackProvider
--             ""         -- model name unused for FallbackProvider
--         }
--
--   -- Option 2: Using a helper (recommended for readability)
--   let chain3 p1 c1 m1 p2 c2 m2 p3 c3 m3 = FallbackProvider
--         (ProviderConfig p1 c1 m1)
--         (ProviderConfig (FallbackProvider (ProviderConfig p2 c2 m2) (ProviderConfig p3 c3 m3)) undefined "")
--
--       threeWayFallback = chain3
--         Qwen qwenCreds "Qwen/Qwen2.5-32B-Instruct"
--         Claude claudeCreds "claude-3-opus"
--         OpenAI gpt5Creds "gpt-5-preview"
--
--   result <- respondJSON threeWayFallback undefined undefined messages schema
--   -- Tries: Qwen → Claude → GPT-5 (stops at first success)
-- @
--
-- The nested approach works because each 'FallbackProvider' is itself a valid provider.
-- This composition pattern extends to any number of providers while maintaining full type safety.
module HaskLLM.FallbackLLM
  ( FallbackProvider (..),
    ProviderConfig (..),

    -- * Ergonomic helpers for multi-provider chains
    chain3,
  )
where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import HaskLLM
  ( Credentials,
    LLMFormatChat (..),
  )

-- | Configuration for a single provider with its credentials and model name.
data ProviderConfig provider = ProviderConfig
  { -- | Provider instance (e.g., Qwen, OpenAI)
    provider :: provider,
    -- | Credentials for this provider
    credentials :: Credentials,
    -- | Model name to use
    modelName :: Text
  }

-- | A fallback provider that tries a primary provider first, then falls back to a secondary.
--   This type itself is an instance of 'LLMFormatChat', allowing it to be used anywhere
--   a regular provider can be used.
data FallbackProvider p1 p2 = FallbackProvider
  { -- | Primary provider (tried first)
    primary :: ProviderConfig p1,
    -- | Secondary provider (fallback)
    secondary :: ProviderConfig p2
  }

-- | Try an IO action and return Nothing on any exception
tryIO :: IO a -> IO (Maybe a)
tryIO action = catch (Just <$> action) (\(_ :: SomeException) -> pure Nothing)

-- | Execute fallback logic: try primary, fall back to secondary on failure
withFallback ::
  (LLMFormatChat p1, LLMFormatChat p2, MonadIO m) =>
  ProviderConfig p1 ->
  ProviderConfig p2 ->
  (forall p. (LLMFormatChat p) => p -> Credentials -> Text -> IO a) ->
  m a
withFallback prim sec action = liftIO $ do
  result <- tryIO (action (provider prim) (credentials prim) (modelName prim))
  case result of
    Just r -> pure r
    Nothing -> action (provider sec) (credentials sec) (modelName sec)

instance (LLMFormatChat p1, LLMFormatChat p2) => LLMFormatChat (FallbackProvider p1 p2) where
  respondText (FallbackProvider prim sec) _ _ msgs =
    withFallback prim sec $ \p c m -> respondText p c m msgs

  respondJSON (FallbackProvider prim sec) _ _ msgs schema =
    withFallback prim sec $ \p c m -> respondJSON p c m msgs schema

  respondTextWithTokens (FallbackProvider prim sec) _ _ msgs mTokens =
    withFallback prim sec $ \p c m -> respondTextWithTokens p c m msgs mTokens

  respondJSONWithTokens (FallbackProvider prim sec) _ _ msgs schema mTokens =
    withFallback prim sec $ \p c m -> respondJSONWithTokens p c m msgs schema mTokens

  respondTextWithConfig (FallbackProvider prim sec) _ _ msgs config =
    withFallback prim sec $ \p c m -> respondTextWithConfig p c m msgs config

  respondJSONWithConfig (FallbackProvider prim sec) _ _ msgs schema config =
    withFallback prim sec $ \p c m -> respondJSONWithConfig p c m msgs schema config

  respondTextWithTokensAndConfig (FallbackProvider prim sec) _ _ msgs mTokens config =
    withFallback prim sec $ \p c m -> respondTextWithTokensAndConfig p c m msgs mTokens config

  respondJSONWithTokensAndConfig (FallbackProvider prim sec) _ _ msgs schema mTokens config =
    withFallback prim sec $ \p c m -> respondJSONWithTokensAndConfig p c m msgs schema mTokens config

--------------------------------------------------------------------------------
-- Ergonomic helpers for multi-provider chains
--------------------------------------------------------------------------------

-- | Chain three providers into a fallback sequence: p1 → p2 → p3
--
-- Example:
-- @
--   let tripleBackup = chain3
--         Phony undefined "phony"          -- Will fail
--         Qwen qwenCreds "Qwen/..."         -- First real attempt
--         OpenAI gpt5Creds "gpt-5-preview"  -- Final fallback
--
--   result <- respondText tripleBackup undefined undefined messages
-- @
chain3 ::
  p1 ->
  Credentials ->
  Text ->
  p2 ->
  Credentials ->
  Text ->
  p3 ->
  Credentials ->
  Text ->
  FallbackProvider p1 (FallbackProvider p2 p3)
chain3 p1 c1 m1 p2 c2 m2 p3 c3 m3 =
  FallbackProvider
    (ProviderConfig p1 c1 m1)
    (ProviderConfig innerFallback undefined "")
  where
    innerFallback = FallbackProvider (ProviderConfig p2 c2 m2) (ProviderConfig p3 c3 m3)
