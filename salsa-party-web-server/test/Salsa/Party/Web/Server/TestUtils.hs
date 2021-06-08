{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.Logger
import Network.HTTP.Client as HTTP
import Path.IO
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Static
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai
import Test.Syd.Yesod

type ServerSpec = YesodSpec App

serverSpec :: ServerSpec -> Spec
serverSpec = managerSpec . yesodSpecWithSiteSetupFunc serverSetupFunc

serverSetupFunc :: HTTP.Manager -> SetupFunc App
serverSetupFunc man = do
  tdir <- tempDirSetupFunc "salsa"
  pool <- connectionPoolSetupFunc migrateAll
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  pure
    App
      { appLogLevel = LevelWarn,
        appStatic = salsaPartyWebServerStatic,
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }
