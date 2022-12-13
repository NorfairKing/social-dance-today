{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Server where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Cache
import qualified Data.Text as T
import Database.Persist.Sqlite
import Lens.Micro
import Network.HTTP.Client.TLS as HTTP
import qualified OpenStreetMaps.Geocoding as OSM
import Path
import Path.IO
import Salsa.Party.DB.Migration
import Salsa.Party.Loopers
import Salsa.Party.OptParse
import Salsa.Party.Poster
import Salsa.Party.Web.Server
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Static
import qualified System.Clock as TimeSpec
import System.Exit
import Text.Colour
import Text.Show.Pretty
import UnliftIO
import Yesod.Core

salsaPartyServer :: IO ()
salsaPartyServer = getSettings >>= runSalsaPartyServer

runSalsaPartyServer :: Settings -> IO ()
runSalsaPartyServer settings@Settings {..} = do
  let info = mkSqliteConnectionInfo (T.pack (fromAbsFile settingDbFile)) & walEnabled .~ False & fkEnabled .~ False
  runMyLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel) $ do
      logInfoN $ T.pack $ ppShow settings
      -- Set this to true momentarily when adding a new poster
      when False convertPosters
      withSqlitePoolInfo info 1 $ \pool -> do
        runSqlPool (completeServerMigration False) pool
        sessionKeyFile <- resolveFile' "client_session_key.aes"
        man <- HTTP.newTlsManager
        rateLimiter <- liftIO $ makeTokenLimiter OSM.tokenLimitConfig
        searchResultCache <- liftIO $ newCache $ Just $ TimeSpec.fromNanoSecs $ (60 * 60 + 5) * 1_000_000_000 -- A bit more than one hour
        exploreResultCache <- liftIO $ newCache $ Just $ TimeSpec.fromNanoSecs $ (6 * 60 * 60 + 5) * 1_000_000_000 -- A bit more than six hours
        let app =
              App
                { appRoot = settingHost,
                  appLogLevel = settingLogLevel,
                  appLogSource = defaultMessageLoggerSource (shouldLogIO app),
                  appStatic = salsaPartyWebServerStatic,
                  appHashDifficulty = 10,
                  appConnectionPool = pool,
                  appHTTPManager = man,
                  appSessionKeyFile = sessionKeyFile,
                  appSecureOnly = True,
                  appSendEmails = settingSendEmails,
                  appSendAddress = settingSendAddress,
                  appProspectSendAddress = settingProspectSendAddress,
                  appSearchResultCache = searchResultCache,
                  appExploreResultCache = exploreResultCache,
                  appAdmin = settingAdmin,
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

runMyLoggingT :: LoggingT IO a -> IO a
runMyLoggingT func =
  if development
    then runLoggingT func developmentLogFunc
    else runStderrLoggingT func
  where
    developmentLogFunc _ src level msg =
      SB8.hPutStrLn stderr $
        fromLogStr $
          mconcat
            [ toLogStr $
                renderChunksUtf8BSBuilder With24BitColours $
                  map
                    (logLevelColour level)
                    [ "[",
                      logLevelChunk level,
                      if T.null src
                        then ""
                        else chunk $ "#" `mappend` src,
                      "]"
                    ],
              " ",
              msg
            ]

logLevelColour :: LogLevel -> (Chunk -> Chunk)
logLevelColour = \case
  LevelDebug -> fore white
  LevelInfo -> fore yellow
  LevelWarn -> fore orange
  LevelError -> fore red
  LevelOther _ -> id
  where
    orange = color256 214

logLevelChunk :: LogLevel -> Chunk
logLevelChunk = \case
  LevelDebug -> "DEBUG"
  LevelInfo -> "INFO"
  LevelWarn -> "WARNING"
  LevelError -> "ERROR"
  LevelOther t -> chunk t

-- We convert the posters before we commit them so that they're already
-- converted by the time we serve them.
convertPosters :: LoggingT IO ()
convertPosters = do
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
