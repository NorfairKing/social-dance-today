{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server where

import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Network.HTTP.Client.TLS as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Path
import Path.IO
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.DB
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
      runSqlPool (runMigration migrateAll) pool
      sessionKeyFile <- resolveFile' "client_session_key.aes"
      man <- HTTP.newTlsManager
      let app =
            App
              { appLogLevel = settingLogLevel,
                appStatic = salsaPartyWebServerStatic,
                appConnectionPool = pool,
                appHTTPManager = man,
                appSessionKeyFile = sessionKeyFile,
                appSendEmails = settingSendEmails,
                appAdmin = settingAdmin,
                appGoogleAPIKey = settingGoogleAPIKey,
                appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
                appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
              }
      liftIO $ do
        waiApp <- Yesod.toWaiAppPlain app
        let loggerMiddle = if development then logStdoutDev else logStdout
        let middles = loggerMiddle . defaultMiddlewaresNoLogging
        let salsaApp = middles waiApp
        Warp.run settingPort salsaApp
