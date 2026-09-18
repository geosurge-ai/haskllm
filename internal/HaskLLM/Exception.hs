-- | Exception boundaries for recoverable failures. Cancellation isn't recovery.
module HaskLLM.Exception (trySync) where

import Control.Exception (SomeAsyncException, SomeException, fromException, tryJust)

trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust \exception -> case fromException exception of
  Just (_ :: SomeAsyncException) -> Nothing
  Nothing -> Just exception
