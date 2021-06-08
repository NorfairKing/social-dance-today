{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist.Sqlite
import Path.IO
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Static
import Test.Syd
import Test.Syd.Yesod

type ServerSpec = YesodSpec App

serverSpec :: ServerSpec -> Spec
serverSpec =
  around
    ( \func -> runNoLoggingT $
        withSqlitePool ":memory:" 1 $ \pool -> do
          runSqlPool (void $ runMigrationQuiet migrateAll) pool
          liftIO $
            withSystemTempDir "salsa" $ \tdir ->
              func (pool, tdir)
    )
    . yesodSpecWithSiteGeneratorAndArgument
      ( \(pool, tdir) -> do
          sessionKeyFile <- resolveFile tdir "session-key.aes"
          pure
            App
              { appLogLevel = LevelWarn,
                appStatic = salsaPartyWebServerStatic,
                appConnectionPool = pool,
                appSessionKeyFile = sessionKeyFile,
                appGoogleAnalyticsTracking = Nothing,
                appGoogleSearchConsoleVerification = Nothing
              }
      )
