{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server where

import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Path
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.OptParse
import Salsa.Party.Web.Server.Static
import Text.Show.Pretty
import Yesod

salsaPartyWebServer :: IO ()
salsaPartyWebServer = do
  sets <- getSettings
  when development $ pPrint sets
  runSalsaPartyWebServer sets

runSalsaPartyWebServer :: Settings -> IO ()
runSalsaPartyWebServer Settings {..} =
  runStderrLoggingT $
    withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
      let app =
            App
              { appLogLevel = settingLogLevel,
                appStatic = salsaPartyWebServerStatic,
                appConnectionPool = pool,
                appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
                appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
              }
      liftIO $ Yesod.warp settingPort app
