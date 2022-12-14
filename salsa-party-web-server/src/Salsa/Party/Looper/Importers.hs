{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.Importers where

import Control.Monad.Logger
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import GHC.Generics (Generic)
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
      Just importerSettings ->
        (,) (importerName importer)
          <$> ( (,)
                  <$> getSoonestRun settingImporterInterval app importerSettings importer
                  <*> pure (importerTimezoneOffset importer)
              )
  now <- liftIO getCurrentTime
  let mImporterName = computeImporterToRun settingImporterInterval now (M.fromList soonestRuns)
  pure $ mImporterName >>= (\name -> find ((== name) . importerName) allImporters)

-- | Compute which importer to run next.
--
-- The overal strategy here is:
--
-- * Any importers that must be run ASAP are run first
--   For those we still prioritise those which are in the preferred time of day range.
-- * We categorise the overdue importers by how much they're overdue:
--   * Between 0 and 1 interval overdue: only run if in their preferred time of day range.
--   * More than 1 interval overdue: run the most overdue one first.
computeImporterToRun :: NominalDiffTime -> UTCTime -> Map a (SoonestRun, Int) -> Maybe a
computeImporterToRun importerInterval now soonestRuns =
  let asap = M.filter ((== RunASAP) . fst) soonestRuns
   in case M.lookupMin asap of
        Just (i, _) -> Just i
        Nothing ->
          let overdues =
                M.mapMaybe
                  ( \(sr, offset) -> case sr of
                      RunNoSoonerThan threshold | now >= threshold -> Just (diffUTCTime now threshold, offset)
                      _ -> Nothing
                  )
                  soonestRuns
              wouldRun (_, (diff, offset)) =
                if diff <= importerInterval
                  then nowIsWithinTimeOfDayRange now (hoursToTimeZone offset)
                  else True
           in case find wouldRun (sortOn (Down . fst . snd) (M.toList overdues)) of
                Nothing -> Nothing -- No loopers
                Just (importer, _) -> Just importer

nowIsWithinTimeOfDayRange :: UTCTime -> TimeZone -> Bool
nowIsWithinTimeOfDayRange now tz =
  let lt = utcToLocalTime tz now
   in timeOfDayWithinRange (localTimeOfDay lt)

timeOfDayWithinRange :: TimeOfDay -> Bool
timeOfDayWithinRange tod =
  let (start, end) = preferredTimeOfDayRange
   in start <= tod && tod < end

-- | Prefer to import between 01 and 13 in the local time of the importer
preferredTimeOfDayRange :: (TimeOfDay, TimeOfDay)
preferredTimeOfDayRange = (TimeOfDay 01 00 00, TimeOfDay 13 00 00)

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
