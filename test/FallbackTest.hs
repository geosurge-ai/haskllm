{-# LANGUAGE OverloadedStrings #-}

module FallbackTest (spec) where

import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Test.Hspec

import HaskLLM (
  ChatMessage (..),
  Credentials (..),
  JSONSchemaSpec (..),
  LLMFormatChat (..),
  LLMResponse (..),
  TokenUsage (..),
 )
import HaskLLM.FallbackLLM (
  FallbackProvider (..),
  ProviderConfig (..),
  chain3,
 )
import HaskLLM.Phony (Phony (..))

--------------------------------------------------------------------------------
-- Mock "Success" provider for testing
--------------------------------------------------------------------------------

-- | A mock provider that always succeeds with predictable responses
data MockSuccess = MockSuccess
  deriving (Show, Eq)

instance LLMFormatChat MockSuccess where
  respondText _ _ modelName _ = pure $ "MockSuccess response from " <> modelName
  respondJSON _ _ modelName _ _ = pure $ object ["mock" .= ("success" :: Text), "model" .= modelName]
  respondTextWithTokens _ _ modelName _ _ = pure $ "MockSuccess response from " <> modelName
  respondJSONWithTokens _ _ modelName _ _ _ = pure $ object ["mock" .= ("success" :: Text), "model" .= modelName]
  respondTextWithConfig _ _ modelName _ _ = pure $ "MockSuccess response from " <> modelName
  respondJSONWithConfig _ _ modelName _ _ _ = pure $ object ["mock" .= ("success" :: Text), "model" .= modelName]
  respondTextWithTokensAndConfig _ _ modelName _ _ _ = pure $ "MockSuccess response from " <> modelName
  respondJSONWithTokensAndConfig _ _ modelName _ _ _ _ = pure $ object ["mock" .= ("success" :: Text), "model" .= modelName]
  respondTextDetailed _ _ modelName _ _ _ =
    pure $
      LLMResponse
        { responseContent = "MockSuccess response from " <> modelName,
          responseUsage = Just mockUsage,
          responseModel = modelName,
          responseProvider = "mock"
        }
  respondJSONDetailed _ _ modelName _ _ _ _ =
    pure $
      LLMResponse
        { responseContent = object ["mock" .= ("success" :: Text), "model" .= modelName],
          responseUsage = Just mockUsage,
          responseModel = modelName,
          responseProvider = "mock"
        }

mockUsage :: TokenUsage
mockUsage =
  TokenUsage
    { inputTokens = Just 10,
      outputTokens = Just 5,
      totalTokens = Just 15,
      cachedInputTokens = Nothing,
      reasoningTokens = Nothing
    }

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "HaskLLM.FallbackLLM" $ do
  let dummyCreds = Credentials $ M.fromList [("dummy", "dummy")]
      testMessages = [ChatMessage "user" "test"]
      testSchema = JSONSchemaSpec "test" (object ["type" .= ("object" :: Text)]) False

  describe "Basic fallback (2 providers)" $ do
    it "falls back from Phony to MockSuccess" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony-model",
                secondary = ProviderConfig MockSuccess dummyCreds "success-model"
              }

      result <- respondText fallback undefined undefined testMessages
      result `shouldBe` "MockSuccess response from success-model"

    it "uses primary when it succeeds" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig MockSuccess dummyCreds "primary-model",
                secondary = ProviderConfig MockSuccess dummyCreds "secondary-model"
              }

      result <- respondText fallback undefined undefined testMessages
      result `shouldBe` "MockSuccess response from primary-model"

    it "fails when both providers fail" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony1",
                secondary = ProviderConfig Phony dummyCreds "phony2"
              }

      result <- try @SomeException (respondText fallback undefined undefined testMessages)
      result `shouldSatisfy` isLeft

  describe "Three-way fallback chain" $ do
    it "skips two Phonies and uses third provider (explicit nesting)" $ do
      -- Explicit nesting approach - most transparent
      let threeWayFallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony1",
                secondary =
                  ProviderConfig
                    ( FallbackProvider
                        { primary = ProviderConfig Phony dummyCreds "phony2",
                          secondary = ProviderConfig MockSuccess dummyCreds "success-model"
                        }
                    )
                    undefined
                    ""
              }

      result <- respondText threeWayFallback undefined undefined testMessages
      result `shouldBe` "MockSuccess response from success-model"

    it "skips two Phonies using chain3 helper (ergonomic)" $ do
      -- Using the ergonomic chain3 helper
      let threeWayFallback =
            chain3
              Phony
              dummyCreds
              "phony1"
              Phony
              dummyCreds
              "phony2"
              MockSuccess
              dummyCreds
              "success-model"

      result <- respondText threeWayFallback undefined undefined testMessages
      result `shouldBe` "MockSuccess response from success-model"

    it "stops at second provider when it succeeds" $ do
      let threeWayFallback =
            chain3
              Phony
              dummyCreds
              "phony1"
              MockSuccess
              dummyCreds
              "second-model"
              MockSuccess
              dummyCreds
              "third-model"

      result <- respondText threeWayFallback undefined undefined testMessages
      result `shouldBe` "MockSuccess response from second-model"

    it "uses first provider when it succeeds (no fallback needed)" $ do
      let threeWayFallback =
            chain3
              MockSuccess
              dummyCreds
              "first-model"
              Phony
              dummyCreds
              "phony"
              MockSuccess
              dummyCreds
              "third-model"

      result <- respondText threeWayFallback undefined undefined testMessages
      result `shouldBe` "MockSuccess response from first-model"

  describe "JSON responses with fallback" $ do
    it "falls back correctly for JSON responses" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony-model",
                secondary = ProviderConfig MockSuccess dummyCreds "success-model"
              }

      result <- respondJSON fallback undefined undefined testMessages testSchema
      result `shouldBe` object ["mock" .= ("success" :: Text), "model" .= ("success-model" :: Text)]

    it "falls back correctly for JSON in three-way chain" $ do
      let threeWayFallback =
            chain3
              Phony
              dummyCreds
              "phony1"
              Phony
              dummyCreds
              "phony2"
              MockSuccess
              dummyCreds
              "success-model"

      result <- respondJSON threeWayFallback undefined undefined testMessages testSchema
      result `shouldBe` object ["mock" .= ("success" :: Text), "model" .= ("success-model" :: Text)]

  describe "Advanced methods with fallback" $ do
    it "falls back correctly for respondTextWithTokens" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony",
                secondary = ProviderConfig MockSuccess dummyCreds "success"
              }

      result <- respondTextWithTokens fallback undefined undefined testMessages (Just 100)
      result `shouldBe` "MockSuccess response from success"

    it "falls back correctly for respondJSONWithTokens" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony",
                secondary = ProviderConfig MockSuccess dummyCreds "success"
              }

      result <- respondJSONWithTokens fallback undefined undefined testMessages testSchema (Just 100)
      result `shouldBe` object ["mock" .= ("success" :: Text), "model" .= ("success" :: Text)]

    it "preserves detailed metadata from the provider that succeeds" $ do
      let fallback =
            FallbackProvider
              { primary = ProviderConfig Phony dummyCreds "phony",
                secondary = ProviderConfig MockSuccess dummyCreds "success"
              }

      result <- respondTextDetailed fallback undefined undefined testMessages (Just 100) undefined
      responseContent result `shouldBe` "MockSuccess response from success"
      responseUsage result `shouldBe` Just mockUsage
      responseModel result `shouldBe` "success"
      responseProvider result `shouldBe` "mock"

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
