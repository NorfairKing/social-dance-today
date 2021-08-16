{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Importer.Selenium where

import Control.Concurrent
import Control.Monad.Logger
import qualified Data.Text as T
import Path
import Path.IO
import System.Process.Typed
import UnliftIO

withSeleniumServer :: (MonadUnliftIO m, MonadLogger m) => m a -> m a
withSeleniumServer func = do
  let processConfig = proc "selenium-server" ["-log", "selenium.log"]
  withProcessTerm processConfig $ \process -> do
    liftIO $ threadDelay 2_000_000
    logInfoN "Starting Selenium server"
    r <- func
    logInfoN "Stopping Selenium server"
    stopProcess process
    pure r

getChromeExecutable :: (MonadIO m, MonadFail m, MonadLogger m) => m (Path Abs File)
getChromeExecutable = do
  chromeFile <- liftIO $ parseRelFile "chromium"
  mChromeExecutable <- findExecutable chromeFile
  case mChromeExecutable of
    Nothing -> fail $ "Could not find executable: " <> fromRelFile chromeFile
    Just chromeExecutable -> do
      logInfoN $ T.pack $ "Found chrome executable at: " <> fromAbsFile chromeExecutable
      pure chromeExecutable
