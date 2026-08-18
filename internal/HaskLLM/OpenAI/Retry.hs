module HaskLLM.OpenAI.Retry (
  OpenAIHttpError (..),
  checkOpenAIResponse,
  checkOpenAIStatus,
  retryOpenAIRequest,
)
where

import Control.Exception (
  Exception,
  SomeAsyncException,
  SomeException,
  catch,
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

retryOpenAIRequest :: Int -> IO a -> IO a
retryOpenAIRequest maxRetries action = go $ max 0 maxRetries
 where
  go retriesLeft =
    catch action $ \(exception :: SomeException) ->
      case fromException exception of
        Just (_ :: SomeAsyncException) -> throwIO exception
        Nothing
          | retriesLeft > 0,
            isRetryable exception ->
              go $ retriesLeft - 1
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
isRetryableStatus code = code == 429 || code >= 500 && code < 600
