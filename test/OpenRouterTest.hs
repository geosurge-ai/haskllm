{-# LANGUAGE OverloadedStrings #-}

module OpenRouterTest (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import Test.Hspec

import HaskLLM (
  ChatMessage (..),
  Credentials (..),
  JSONSchemaSpec (..),
  LLMFormatChat (..),
  LLMResponse (..),
  RequestConfig (..),
  TokenUsage (..),
  defaultRequestConfig,
 )
import HaskLLM.Internal (extractChatContent, stripCodeFence)
import HaskLLM.OpenRouter (OpenRouter (..), chatCompletionsBody)

model :: Text
model = "z-ai/glm-5.3-flash"

routing :: Value
routing = object ["order" .= ["Reka", "Sail Research" :: Text], "allow_fallbacks" .= False]

answerSchema :: Value
answerSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties" .= object ["answer" .= object ["type" .= ("string" :: Text)]],
      "required" .= ["answer" :: Text],
      "additionalProperties" .= False
    ]

spec :: Spec
spec = do
  describe "OpenRouter request body" $ do
    let msgs = [ChatMessage "user" "hi"]
    it "sends routing, response_format and max_tokens only when given" $ do
      chatCompletionsBody (OpenRouter (Just routing)) model msgs (Just (JSONSchemaSpec "answer" answerSchema True)) (Just 100)
        `shouldBe` object
          [ "model" .= model,
            "messages" .= msgs,
            "provider" .= routing,
            "response_format"
              .= object
                [ "type" .= ("json_schema" :: Text),
                  "json_schema" .= object ["name" .= ("answer" :: Text), "schema" .= answerSchema, "strict" .= True]
                ],
            "max_tokens" .= (100 :: Int)
          ]
      chatCompletionsBody (OpenRouter Nothing) model msgs Nothing Nothing
        `shouldBe` object ["model" .= model, "messages" .= msgs]

  describe "Chat Completions response parsing" $ do
    let choice fields = object ["choices" .= [object fields]]
    it "returns the first choice's content" $
      extractChatContent (choice ["finish_reason" .= ("stop" :: Text), "message" .= object ["content" .= ("pong" :: Text)]])
        `shouldBe` Right "pong"
    it "refuses error bodies and truncated completions served as HTTP 200" $ do
      extractChatContent (object ["error" .= object ["message" .= ("boom" :: Text)]]) `shouldSatisfy` isLeft
      extractChatContent (choice ["finish_reason" .= ("error" :: Text), "error" .= object ["code" .= (502 :: Int)], "message" .= object ["content" .= ("" :: Text)]]) `shouldSatisfy` isLeft
      extractChatContent (choice ["finish_reason" .= ("length" :: Text), "message" .= object ["content" .= Null]]) `shouldSatisfy` isLeft
    it "strips single-line and multi-line code fences" $ do
      stripCodeFence "```json {\"a\":1}```" `shouldBe` "{\"a\":1}"
      stripCodeFence "```json\n{\"a\":1}\n```" `shouldBe` "{\"a\":1}"
      stripCodeFence "{\"a\":1}" `shouldBe` "{\"a\":1}"

  describe "OpenRouter integration" $ do
    it "answers through the live API and reports usage" $ withKey $ do
      response <-
        respondTextDetailed provider (Credentials mempty) model [ChatMessage "user" "Reply with the single word: pong"] (Just 2048) config
      T.toLower (responseContent response) `shouldSatisfy` T.isInfixOf "pong"
      (responseUsage response >>= costUsd) `shouldSatisfy` isJust
    it "enforces a JSON schema through the live API" $ withKey $ do
      response <-
        respondJSONDetailed
          provider
          (Credentials mempty)
          model
          [ChatMessage "user" "What colour is the sky on a clear day? Answer in one word."]
          (JSONSchemaSpec "answer" answerSchema True)
          (Just 2048)
          config
      case responseContent response of
        Object o -> KM.lookup "answer" o `shouldSatisfy` isJust
        other -> expectationFailure ("expected an object, got " <> show other)
 where
  provider = OpenRouter (Just routing)
  config = defaultRequestConfig {timeoutSeconds = Just 120, maxRetries = 0}
  withKey action =
    lookupEnv "OPENROUTER_API_KEY" >>= maybe (pendingWith "OPENROUTER_API_KEY not set") (const action)
