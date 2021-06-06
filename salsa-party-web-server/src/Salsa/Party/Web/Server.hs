{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server where

import Control.Monad
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
runSalsaPartyWebServer Settings {..} = do
  let app =
        App
          { appLogLevel = settingLogLevel,
            appStatic = salsaPartyWebServerStatic,
            appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
          }
  Yesod.warp settingPort app
