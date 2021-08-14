module Salsa.Party.Importer.Selenium where

import System.Process.Typed
import UnliftIO

withSeleniumServer :: MonadUnliftIO m => m a -> m a
withSeleniumServer func = do
  let processConfig = proc "selenium-server" ["-log", "selenium.log"]
  withProcessTerm processConfig $ \process -> do
    func
