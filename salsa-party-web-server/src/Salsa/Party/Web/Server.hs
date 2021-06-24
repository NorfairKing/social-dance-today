{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server where

import Control.Concurrent.TokenLimiter
import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import GHC.Clock (getMonotonicTimeNSec)
import Lens.Micro
import Looper
import Network.HTTP.Client.TLS as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import qualified OpenStreetMaps.Geocoding as OSM
import Path
import Path.IO
import Salsa.Party.DB.Migration
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.SalsaCH
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.OptParse
import Salsa.Party.Web.Server.Static
import Text.Printf
import Text.Show.Pretty
import UnliftIO
import Yesod

salsaPartyWebServer :: IO ()
salsaPartyWebServer = do
  settings <- getSettings
  when development $ pPrint settings
  runSalsaPartyWebServer settings

runSalsaPartyWebServer :: Settings -> IO ()
runSalsaPartyWebServer Settings {..} = do
  let info = mkSqliteConnectionInfo (T.pack (fromAbsFile settingDbFile)) & walEnabled .~ False & fkEnabled .~ False
  runStderrLoggingT $
    withSqlitePoolInfo info 1 $ \pool -> do
      runSqlPool (completeServerMigration False) pool
      sessionKeyFile <- resolveFile' "client_session_key.aes"
      man <- HTTP.newTlsManager
      rateLimiter <- liftIO $ newRateLimiter OSM.limitConfig

      let app =
            App
              { appLogLevel = settingLogLevel,
                appStatic = salsaPartyWebServerStatic,
                appConnectionPool = pool,
                appHTTPManager = man,
                appSessionKeyFile = sessionKeyFile,
                appSendEmails = settingSendEmails,
                appAdmin = settingAdmin,
                appOSMRateLimiter = do
                  guard settingEnableOSMGeocoding
                  pure rateLimiter,
                appGoogleAPIKey = do
                  guard settingEnableGoogleGeocoding
                  settingGoogleAPIKey,
                appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
                appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
              }
      let looperDefs = [mkLooperDef "importer-events.info" settingEventsInfoImportLooperSettings (runImporter app runSalsaCHImporter)]
          looperRunner LooperDef {..} = do
            logInfoNS looperDefName "Starting"
            begin <- liftIO getMonotonicTimeNSec
            looperDefFunc
            end <- liftIO getMonotonicTimeNSec
            logInfoNS looperDefName $ T.pack $ printf "Done, took %.2f seconds" (fromIntegral (end - begin) / (1_000_000_000 :: Double))
      let runTheLoopers = runLoopersIgnoreOverrun looperRunner looperDefs
      let runTheServer = liftIO $ do
            waiApp <- Yesod.toWaiAppPlain app
            let loggerMiddle = if development then logStdoutDev else logStdout
            let middles = loggerMiddle . defaultMiddlewaresNoLogging
            let salsaApp = middles waiApp
            Warp.run settingPort salsaApp
      concurrently_ runTheLoopers runTheServer
