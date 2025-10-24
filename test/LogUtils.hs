{-# LANGUAGE OverloadedStrings #-}

-- | Professional logging utilities with environment variable configuration
module LogUtils (
  logDebug,
  logInfo,
  logWarning,
  logError,
  logSection,
  withLogSection,
)
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toUpper)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Log levels in order of severity
data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Eq, Ord, Show)

-- | Global logging configuration based on HASKLLM_LOG_LEVEL environment variable
-- Supported levels: DEBUG, INFO, WARNING, ERROR (default: DEBUG for maximum verbosity)
{-# NOINLINE globalLogLevel #-}
globalLogLevel :: LogLevel
globalLogLevel = unsafePerformIO $ do
  logLevelEnv <- lookupEnv "HASKLLM_LOG_LEVEL"
  let logLevel = maybe "DEBUG" (map toUpper) logLevelEnv
  pure $ case logLevel of
    "DEBUG" -> DEBUG
    "INFO" -> INFO
    "WARNING" -> WARNING
    "ERROR" -> ERROR
    _ -> DEBUG

-- | Check if a log level should be logged
shouldLog :: LogLevel -> Bool
shouldLog level = level >= globalLogLevel

-- | Log a debug message
logDebug :: (MonadIO m) => Text -> m ()
logDebug msg =
  when (shouldLog DEBUG) $
    liftIO $
      putStrLn $
        "[DEBUG] " <> T.unpack msg

-- | Log an info message
logInfo :: (MonadIO m) => Text -> m ()
logInfo msg =
  when (shouldLog INFO) $
    liftIO $
      putStrLn $
        "[INFO] " <> T.unpack msg

-- | Log a warning message
logWarning :: (MonadIO m) => Text -> m ()
logWarning msg =
  when (shouldLog WARNING) $
    liftIO $
      putStrLn $
        "[WARNING] " <> T.unpack msg

-- | Log an error message
logError :: (MonadIO m) => Text -> m ()
logError msg =
  when (shouldLog ERROR) $
    liftIO $
      putStrLn $
        "[ERROR] " <> T.unpack msg

-- | Log a section header with decorative formatting
logSection :: (MonadIO m) => Text -> m ()
logSection title = do
  let border = T.replicate (T.length title + 8) "="
  logInfo border
  logInfo $ "=== " <> title <> " ==="
  logInfo border

-- | Execute an action within a logged section
withLogSection :: (MonadIO m) => Text -> m a -> m a
withLogSection title action = do
  logSection title
  result <- action
  logInfo ""
  pure result
