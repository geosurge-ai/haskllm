{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the OpenAI tool-calling loop, using a scripted transport
--   instead of the network.
module ToolLoopTest (spec) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import HaskLLM (TokenUsage (..))
import HaskLLM.OpenAI.GPT5 (
  Tool (..),
  ToolChatResult (..),
  ToolInvocation (..),
  ToolSpec (..),
  runToolLoop,
 )
import HaskLLM.OpenAI.Retry (OpenAIHttpError (..), retryOpenAIRequest)
import HaskLLM.Tools (aggregateUsage)

--------------------------------------------------------------------------------
-- A simple tool under test

data AddArgs = AddArgs Int Int

instance FromJSON AddArgs where
  parseJSON = withObject "AddArgs" $ \o -> AddArgs <$> o .: "x" <*> o .: "y"

addToolSpec :: ToolSpec AddArgs
addToolSpec =
  ToolSpec
    { toolName = "add",
      toolDescription = "Add two integers",
      toolSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "x" .= object ["type" .= ("integer" :: Text)],
                  "y" .= object ["type" .= ("integer" :: Text)]
                ],
            "required" .= (["x", "y"] :: [Text]),
            "additionalProperties" .= False
          ],
      toolStrict = True
    }

addTool :: Tool
addTool = Tool addToolSpec (\(AddArgs x y) -> pure (T.pack (show (x + y))))

--------------------------------------------------------------------------------
-- Scripted Responses-API payloads

usageValue :: Int -> Int -> Value
usageValue inp out =
  object
    [ "input_tokens" .= inp,
      "output_tokens" .= out,
      "total_tokens" .= (inp + out)
    ]

-- | A response whose output is plain assistant text.
textResponse :: Text -> Value
textResponse t =
  object
    [ "output"
        .= [ object
               [ "type" .= ("message" :: Text),
                 "content" .= [object ["type" .= ("output_text" :: Text), "text" .= t]]
               ]
           ],
      "usage" .= usageValue 10 5
    ]

-- | A @reasoning@ output item, as emitted by reasoning models (e.g. GPT-5).
--   The loop must echo these back verbatim when continuing a tool turn.
reasoningItem :: Text -> Value
reasoningItem rid =
  object
    [ "type" .= ("reasoning" :: Text),
      "id" .= rid,
      "summary" .= ([] :: [Value])
    ]

-- | A single @function_call@ output item.
callItem :: Text -> Text -> Text -> Value
callItem callId nm args =
  object
    [ "type" .= ("function_call" :: Text),
      "call_id" .= callId,
      "name" .= nm,
      "arguments" .= args
    ]

-- | A response consisting of the given @function_call@ items.
callResponse :: [Value] -> Value
callResponse items = object ["output" .= items, "usage" .= usageValue 20 7]

-- | The expected @function_call_output@ item the loop should feed back.
outputItem :: Text -> Text -> Value
outputItem callId out =
  object
    [ "type" .= ("function_call_output" :: Text),
      "call_id" .= callId,
      "output" .= out
    ]

userItem :: Text -> Value
userItem t = object ["role" .= ("user" :: Text), "content" .= t]

-- | A transport that pops scripted responses and records every input it saw.
mkTransport :: [Value] -> IO (IORef [[Value]], [Value] -> IO Value)
mkTransport responses = do
  remaining <- newIORef responses
  seen <- newIORef []
  let transport items = do
        modifyIORef' seen (<> [items])
        rs <- readIORef remaining
        case rs of
          [] -> fail "scripted transport exhausted"
          (r : rest) -> writeIORef remaining rest >> pure r
  pure (seen, transport)

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "OpenAI tool loop (scripted transport)" $ do
  it "returns final text and usage when the model makes no tool calls" $ do
    (_, transport) <- mkTransport [textResponse "hello"]
    (result, usages) <- runToolLoop transport [addTool] [userItem "hi"] 5
    finalText result `shouldBe` "hello"
    toolTrace result `shouldBe` []
    length usages `shouldBe` 1

  it "echoes the full prior output (including reasoning items) with tool outputs appended" $ do
    let reasoning = reasoningItem "rs_1"
        call = callItem "call_1" "add" "{\"x\":2,\"y\":3}"
    (seen, transport) <- mkTransport [callResponse [reasoning, call], textResponse "the answer is 5"]
    (result, usages) <- runToolLoop transport [addTool] [userItem "add 2 3"] 5

    finalText result `shouldBe` "the answer is 5"
    toolTrace result `shouldBe` [ToolInvocation "add" "{\"x\":2,\"y\":3}" "5"]

    inputs <- readIORef seen
    length inputs `shouldBe` 2
    inputs !! 1 `shouldBe` [userItem "add 2 3", reasoning, call, outputItem "call_1" "5"]

    let usage = aggregateUsage usages
    (usage >>= inputTokens) `shouldBe` Just 30
    (usage >>= outputTokens) `shouldBe` Just 12
    (usage >>= cachedInputTokens) `shouldBe` Nothing

  it "executes multiple calls from a single response in order" $ do
    let c1 = callItem "c1" "add" "{\"x\":1,\"y\":2}"
        c2 = callItem "c2" "add" "{\"x\":3,\"y\":4}"
    (seen, transport) <- mkTransport [callResponse [c1, c2], textResponse "done"]
    (result, _) <- runToolLoop transport [addTool] [] 5

    map invokedOutput (toolTrace result) `shouldBe` ["3", "7"]
    inputs <- readIORef seen
    inputs !! 1 `shouldBe` [c1, c2, outputItem "c1" "3", outputItem "c2" "7"]

  it "does not replay a tool handler when its follow-up request retries" $ do
    handlerCalls <- newIORef (0 :: Int)
    attempts <- newIORef (0 :: Int)
    let countedAddTool = Tool addToolSpec $ \(AddArgs x y) -> do
          modifyIORef' handlerCalls (+ 1)
          pure $ T.pack $ show $ x + y
        transport _ = retryOpenAIRequest 1 $ do
          modifyIORef' attempts (+ 1)
          attempt <- readIORef attempts
          case attempt of
            1 -> pure $ callResponse [callItem "c1" "add" "{\"x\":2,\"y\":3}"]
            2 -> throwIO $ OpenAIHttpError 503 mempty
            _ -> pure $ textResponse "done"

    result <- fst <$> runToolLoop transport [countedAddTool] [] 5

    finalText result `shouldBe` "done"
    readIORef handlerCalls `shouldReturn` 1
    readIORef attempts `shouldReturn` 3

  it "feeds unknown-tool errors back to the model" $ do
    (seen, transport) <- mkTransport [callResponse [callItem "c1" "bogus" "{}"], textResponse "recovered"]
    (result, _) <- runToolLoop transport [addTool] [] 5

    finalText result `shouldBe` "recovered"
    map invokedOutput (toolTrace result) `shouldBe` ["Error: unknown tool: bogus"]
    inputs <- readIORef seen
    inputs !! 1 `shouldBe` [callItem "c1" "bogus" "{}", outputItem "c1" "Error: unknown tool: bogus"]

  it "feeds argument parse errors back to the model" $ do
    (_, transport) <- mkTransport [callResponse [callItem "c1" "add" "{\"x\":\"nope\"}"], textResponse "recovered"]
    (result, _) <- runToolLoop transport [addTool] [] 5

    finalText result `shouldBe` "recovered"
    case toolTrace result of
      [inv] -> invokedOutput inv `shouldSatisfy` T.isPrefixOf "Error: invalid arguments for add"
      other -> expectationFailure ("unexpected trace: " <> show other)

  it "fails when the round cap is exceeded" $ do
    (_, transport) <- mkTransport (replicate 3 (callResponse [callItem "c1" "add" "{\"x\":1,\"y\":1}"]))
    runToolLoop transport [addTool] [] 2 `shouldThrow` anyIOException
