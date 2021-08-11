{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Server where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Text as T
import Database.Persist.Sqlite
import Lens.Micro
import Network.HTTP.Client.TLS as HTTP
import qualified OpenStreetMaps.Geocoding as OSM
import Path
import Path.IO
import Salsa.Party.DB.Migration
import Salsa.Party.Looper
import Salsa.Party.OptParse
import Salsa.Party.Web.Server
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Poster
import Salsa.Party.Web.Server.Static
import System.Exit
import Text.Show.Pretty
import UnliftIO

salsaPartyServer :: IO ()
salsaPartyServer = do
  settings <- getSettings
  when development $ pPrint settings
  runSalsaPartyServer settings

runSalsaPartyServer :: Settings -> IO ()
runSalsaPartyServer settings@Settings {..} = do
  let info = mkSqliteConnectionInfo (T.pack (fromAbsFile settingDbFile)) & walEnabled .~ False & fkEnabled .~ False
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel) $ do
      -- Set this to true momentarily when adding a new poster
      when development $ convertPosters settingStaticDir
      withSqlitePoolInfo info 1 $ \pool -> do
        runSqlPool (completeServerMigration False) pool
        sessionKeyFile <- resolveFile' "client_session_key.aes"
        man <- HTTP.newTlsManager
        rateLimiter <- liftIO $ makeTokenLimiter OSM.tokenLimitConfig

        let app =
              App
                { appRoot = settingHost,
                  appLogLevel = settingLogLevel,
                  appStatic = salsaPartyWebServerStatic,
                  appConnectionPool = pool,
                  appHTTPManager = man,
                  appSessionKeyFile = sessionKeyFile,
                  appSendEmails = settingSendEmails,
                  appSendAddress = settingSendAddress,
                  appAdmin = settingAdmin,
                  appStaticDir = settingStaticDir,
                  appOSMRateLimiter = do
                    guard settingEnableOSMGeocoding
                    pure rateLimiter,
                  appGoogleAPIKey = do
                    guard settingEnableGoogleGeocoding
                    settingGoogleAPIKey,
                  appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
                  appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification,
                  appSentrySettings = settingSentrySettings
                }
        concurrently_
          (runLoopers settings app)
          (runSalsaPartyWebServer settings app)

-- We convert the posters before we commit them so that they're already
-- converted by the time we serve them.
convertPosters :: Path Abs Dir -> LoggingT IO ()
convertPosters staticDir = do
  locationsDir <- resolveDir staticDir "locations"
  fs <- liftIO $ snd <$> listDirRecur locationsDir
  forM_ fs $ \posterFile -> do
    logDebugN $ T.pack $ "Converting poster: " <> fromAbsFile posterFile
    (name, ext) <- liftIO $ splitExtension posterFile
    mimeType <- case ext of
      ".jpg" -> pure "image/jpeg"
      ".jpeg" -> pure "image/jpeg"
      ".png" -> pure "image/png"
      e -> liftIO $ die $ "Unknown extension: " <> e
    contents <- liftIO $ SB.readFile $ fromAbsFile posterFile
    case posterCropImage mimeType contents of
      Left err -> liftIO $ die err
      Right (convertedMimeType, convertedContents) -> do
        when (convertedMimeType /= "image/jpeg") $ liftIO $ die "Should have been converted to jpg"
        when (convertedContents /= contents) $ do
          convertedPath <- liftIO $ addExtension ext name
          liftIO $ do
            removeFile posterFile
            SB.writeFile (fromAbsFile convertedPath) convertedContents
