{-# LANGUAGE OverloadedStrings #-}

module QwenIntegrationTest (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import HaskLLM
  ( ChatMessage (..),
    Credentials (..),
    LLMFormatChat (..),
  )
import HaskLLM.VLLM.QweN2_5 (Qwen (..))
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = describe "Qwen Integration (blood-money)" $ do
  it "connects to production Qwen and responds to hello world" $ do
    -- Check if BLOOD_MONEY_API_KEY is available
    mApiKey <- lookupEnv "BLOOD_MONEY_API_KEY"
    case mApiKey of
      Nothing -> pendingWith "BLOOD_MONEY_API_KEY not set (run via run-test.sh)"
      Just _ -> do
        -- Use empty credentials - will fall back to env vars and default URL
        let creds = Credentials $ M.fromList []
            messages = [ChatMessage "user" "Hello! Please respond with a brief greeting."]
            modelName = "Qwen/Qwen2.5-32B-Instruct"

        -- Make actual request to production
        response <- respondText Qwen creds modelName messages

        -- Verify we got a non-empty response
        response `shouldSatisfy` (not . T.null)
