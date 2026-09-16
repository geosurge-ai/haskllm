module HaskLLM.OpenAI.Retry (
  OpenAIHttpError (..),
  checkOpenAIResponse,
  checkOpenAIStatus,
  retryOpenAIRequest,
)
where

import Control.Concurrent (threadDelay)
import Control.Exception (
  Exception,
  SomeException,
  fromException,
  throwIO,
 )
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
  HttpException (..),
  HttpExceptionContent (..),
  Response,
  responseBody,
  responseStatus,
 )
import Network.HTTP.Types.Status (statusCode)

import HaskLLM.Exception (trySync)

data OpenAIHttpError = OpenAIHttpError Int ByteString
  deriving (Show)

instance Exception OpenAIHttpError

checkOpenAIResponse :: Response ByteString -> IO (Response ByteString)
checkOpenAIResponse response = do
  checkOpenAIStatus (statusCode $ responseStatus response) $ responseBody response
  pure response

checkOpenAIStatus :: Int -> ByteString -> IO ()
checkOpenAIStatus code body
  | code >= 200 && code < 300 = pure ()
  | otherwise =
      throwIO $
        OpenAIHttpError code $
          LBS.take 4096 body

-- ponytail: fixed 1s/2s/4s backoff capped at 30s, honour Retry-After once 429s show up in logs.
retryOpenAIRequest :: Int -> IO a -> IO a
retryOpenAIRequest maxRetries action = go (max 0 maxRetries) 1
 where
  go retriesLeft delaySeconds =
    trySync action >>= \case
      Right result -> pure result
      Left exception
        | retriesLeft > 0,
          isRetryable exception -> do
            threadDelay (delaySeconds * 1_000_000)
            go (retriesLeft - 1) (min 30 (delaySeconds * 2))
        | otherwise -> throwIO exception

isRetryable :: SomeException -> Bool
isRetryable exception = case fromException exception of
  Just (OpenAIHttpError code _) -> isRetryableStatus code
  Nothing -> case fromException exception of
    Just (HttpExceptionRequest _ (StatusCodeException response _)) ->
      isRetryableStatus $ statusCode $ responseStatus response
    Just (_ :: HttpException) -> True
    Nothing -> False

isRetryableStatus :: Int -> Bool
isRetryableStatus code = code == 408 || code == 429 || code >= 500 && code < 600
