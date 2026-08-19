module OpenAIRetryTest (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Timeout (timeout)
import Test.Hspec

import HaskLLM.OpenAI.Retry (
  OpenAIHttpError (..),
  checkOpenAIStatus,
  retryOpenAIRequest,
 )

spec :: Spec
spec = describe "OpenAI request retries" $ do
  it "propagates cancellation without retrying" $ do
    attempts <- newIORef (0 :: Int)
    let request = modifyIORef' attempts (+ 1) >> throwIO ThreadKilled
    retryOpenAIRequest 3 request `shouldThrow` (== ThreadKilled)
    readIORef attempts `shouldReturn` 1

  forM_ [429, 503] $ \status ->
    it ("retries HTTP " <> show status) $ do
      attempts <- newIORef (0 :: Int)
      let request = do
            modifyIORef' attempts (+ 1)
            attempt <- readIORef attempts
            checkOpenAIStatus (if attempt == 1 then status else 200) mempty
      retryOpenAIRequest 3 request
      readIORef attempts `shouldReturn` 2

  it "does not retry other 4xx responses" $ do
    attempts <- newIORef (0 :: Int)
    let request = modifyIORef' attempts (+ 1) >> checkOpenAIStatus 400 mempty
    retryOpenAIRequest 3 request
      `shouldThrow` (\(OpenAIHttpError status _) -> status == 400)
    readIORef attempts `shouldReturn` 1

  it "lets an outer timeout terminate a delayed request" $ do
    attempts <- newIORef (0 :: Int)
    timedOut <- timeout 50_000 $ retryOpenAIRequest 3 $ do
      modifyIORef' attempts (+ 1)
      threadDelay 1_000_000
    timedOut `shouldBe` Nothing
    readIORef attempts `shouldReturn` 1
