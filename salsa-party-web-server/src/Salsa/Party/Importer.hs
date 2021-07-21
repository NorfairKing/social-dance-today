{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer where

import Control.Exception (AsyncException)
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import GHC.Clock (getMonotonicTimeNSec)
import Looper
import Salsa.Party.DB
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.EventsInfo
import Salsa.Party.Importer.GolatindanceCom
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Text.Printf
import UnliftIO

runImporterLoopers :: Settings -> App -> LoggingT IO ()
runImporterLoopers Settings {..} app = do
  let importerLooper :: Importer -> LooperSettings -> LooperDef (LoggingT IO)
      importerLooper importer sets =
        mkLooperDef
          ("importer-" <> importerName importer)
          sets
          (runImporter app importer)
      looperDefs =
        [ importerLooper eventsInfoImporter settingEventsInfoImportLooperSettings,
          importerLooper golatindanceComImporter settingGolatindanceComImportLooperSettings
        ]
      runDBHere :: SqlPersistT (LoggingT IO) a -> LoggingT IO a
      runDBHere = flip runSqlPool (appConnectionPool app)
      -- TODO move the looper running double-check into the importers
      -- Make sure to get rid of the log sources when we do that.
      looperRunner LooperDef {..} = do
        -- We double-check whether to run the importer because we don't want to
        -- bash any external sites should the importers or the webserver
        -- crashloop, or we just deploy more often than once a day.
        logInfoNS looperDefName "Checking whether to run"
        now <- liftIO getCurrentTime
        mImporterMetadata <- runDBHere $ getBy $ UniqueImporterMetadataName looperDefName
        let mLastRun = importerMetadataLastRunStart . entityVal <$> mImporterMetadata
        shouldRun <- case mLastRun of
          Nothing -> do
            logDebugNS looperDefName "Definitely running because it's never run before"
            pure True
          Just lastRun -> do
            let diff = diffUTCTime now lastRun
            let shouldRun = diff >= looperDefPeriod
                showDiffTime = T.pack . printf "%.0f" . (realToFrac :: NominalDiffTime -> Double)
            let ctx =
                  T.unwords
                    [ "because the last run was",
                      T.pack (show lastRun),
                      "which is",
                      showDiffTime diff,
                      "seconds ago and the looper period is",
                      showDiffTime looperDefPeriod,
                      "seconds"
                    ]
            if shouldRun
              then logDebugNS looperDefName $ "Running " <> ctx
              else logDebugNS looperDefName $ "Not running " <> ctx
            pure shouldRun
        if shouldRun
          then do
            logInfoNS looperDefName "Starting"
            begin <- liftIO getMonotonicTimeNSec
            errOrUnit <-
              (Right <$> looperDefFunc)
                `catches` [
                            -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
                            Handler (\e -> throwIO (e :: AsyncException)),
                            -- Catch all the rest as a string
                            Handler (\e -> return $ Left (e :: SomeException))
                          ]
            end <- liftIO getMonotonicTimeNSec
            case errOrUnit of
              Right () -> pure ()
              Left err -> logErrorNS looperDefName $ "Looper threw an exception:\n" <> T.pack (displayException err)
            logInfoNS looperDefName $ T.pack $ printf "Done, took %.2f seconds" (fromIntegral (end - begin) / (1_000_000_000 :: Double))
          else pure ()

  runLoopersIgnoreOverrun looperRunner looperDefs
