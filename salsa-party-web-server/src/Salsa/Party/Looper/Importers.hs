{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.Importers where

import Control.Monad.Logger
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Safe
import Salsa.Party.Importer
import Salsa.Party.Importers
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()

importersLooper :: Settings -> App -> LoggingT IO ()
importersLooper Settings {..} app = do
  soonestRuns <- forM allImporters $ \importer ->
    case M.lookup (importerName importer) settingImporterSettings of
      Nothing -> fail $ unwords ["Failed to configure importer:", show (importerName importer)]
      Just importerSettings -> (,) importer <$> getSoonestRun settingImporterInterval app importerSettings importer
  case minimumByMay (comparing snd) soonestRuns of
    Nothing -> pure () -- No loopers
    Just (importer, soonestRun) -> do
      logDebugN $ T.pack $ unwords ["Chosen importer to run was:", show (importerName importer), show soonestRun]
      let run = runImporter app importer
      case soonestRun of
        DontRun -> pure () -- Soonest is "don't run", don't do anything.
        RunASAP -> run
        RunNoSoonerThan threshold -> do
          now <- liftIO getCurrentTime
          when (now >= threshold) run

data SoonestRun
  = RunASAP
  | RunNoSoonerThan !UTCTime
  | DontRun
  deriving (Show, Eq, Ord)

getSoonestRun :: NominalDiffTime -> App -> ImporterSettings -> Importer -> LoggingT IO SoonestRun
getSoonestRun importerInterval app ImporterSettings {..} importer =
  if importerSettingEnabled
    then do
      let runDBHere :: SqlPersistT (LoggingT IO) a -> LoggingT IO a
          runDBHere = flip runSqlPool (appConnectionPool app) . retryOnBusy
      importerMetadataEntity <-
        runDBHere $
          upsertBy
            (UniqueImporterMetadataName $ importerName importer)
            ( ImporterMetadata
                { importerMetadataName = importerName importer,
                  importerMetadataLastRunStart = Nothing,
                  importerMetadataLastRunEnd = Nothing,
                  importerMetadataLastRunImported = Nothing
                }
            )
            []
      let mLastRun = importerMetadataLastRunStart $ entityVal importerMetadataEntity
      case mLastRun of
        Nothing -> pure RunASAP
        Just lastRun -> pure $ RunNoSoonerThan $ addUTCTime importerInterval lastRun
    else pure DontRun
