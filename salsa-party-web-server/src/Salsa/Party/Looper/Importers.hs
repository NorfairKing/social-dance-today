{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.Importers where

import Control.Monad.Logger
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Safe
import Salsa.Party.Importer
import Salsa.Party.Importers
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()

importersLooper :: Settings -> App -> LoggingT IO ()
importersLooper settings app = do
  mImporter <- chooseImporterToRun settings app
  forM_ mImporter $ \importer -> do
    logDebugN $
      T.pack $
        unwords
          [ "Chosen importer to run was:",
            show (importerName importer)
          ]
    runImporter app importer

chooseImporterToRun :: Settings -> App -> LoggingT IO (Maybe Importer)
chooseImporterToRun Settings {..} app = do
  soonestRuns <- forM allImporters $ \importer ->
    case M.lookup (importerName importer) settingImporterSettings of
      Nothing -> fail $ unwords ["Failed to configure importer:", show (importerName importer)]
      Just importerSettings -> (,) importer <$> getSoonestRun settingImporterInterval app importerSettings importer
  now <- liftIO getCurrentTime
  pure $ computeImporterToRun now soonestRuns

computeImporterToRun :: UTCTime -> [(a, SoonestRun)] -> Maybe a
computeImporterToRun now soonestRuns =
  case minimumByMay (comparing snd) soonestRuns of
    Nothing -> Nothing -- No loopers
    Just (importer, soonestRun) -> do
      case soonestRun of
        DontRun -> Nothing -- Soonest is "don't run", don't do anything.
        RunASAP -> Just importer
        RunNoSoonerThan threshold ->
          if now >= threshold
            then Just importer
            else Nothing

data SoonestRun
  = RunASAP
  | RunNoSoonerThan !UTCTime
  | DontRun
  deriving (Show, Eq, Ord, Generic)

instance Validity SoonestRun

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
