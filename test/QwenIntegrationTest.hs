{-# LANGUAGE OverloadedStrings #-}

module QwenIntegrationTest (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)
import Test.Hspec

import HaskLLM (
  ChatMessage (..),
  Credentials (..),
  LLMFormatChat (..),
 )
import HaskLLM.VLLM.QweN2_5 (Qwen (..))

spec :: Spec
spec = describe "Qwen Integration (blood-money)" $ do
  it "connects to production Qwen and responds to hello world" $ do
    -- Use empty credentials - will fall back to env vars and default URL
    -- If BLOOD_MONEY_API_KEY is not set, this will fail fast during the request
    let creds = Credentials $ M.empty
        messages = [ChatMessage "user" "Hello! Please respond with a brief greeting."]
        modelName = "Qwen/Qwen2.5-32B-Instruct"

    -- Make actual request to production
    response <- respondText Qwen creds modelName messages

    hPutStrLn stderr ""
    hPutStrLn stderr "Response:"
    hPutStrLn stderr response

    -- Verify we got a non-empty response
    response `shouldSatisfy` (not . T.null)
