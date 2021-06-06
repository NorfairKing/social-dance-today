{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist.Sqlite
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Static
import Test.Hspec
import Yesod.Test

type SalsaPartyWebServerSpec = YesodSpec App

type SalsaPartyWebServerExample = YesodExample App

salsaPartyWebServerSpec :: SalsaPartyWebServerSpec -> Spec
salsaPartyWebServerSpec =
  around (\func -> runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> liftIO $ func pool)
    . yesodSpecWithSiteGeneratorAndArgument
      ( \pool ->
          pure
            App
              { appLogLevel = LevelWarn,
                appStatic = salsaPartyWebServerStatic,
                appConnectionPool = pool,
                appGoogleAnalyticsTracking = Nothing,
                appGoogleSearchConsoleVerification = Nothing
              }
      )
