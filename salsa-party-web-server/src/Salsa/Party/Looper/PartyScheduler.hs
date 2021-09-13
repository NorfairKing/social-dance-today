{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.PartyScheduler
  ( runPartyScheduler,
    ScheduleDecision (..),
    makeScheduleDecision,
    handleScheduleDecision,
    daysToScheduleAhead,
  )
where

import qualified Data.Conduit.Combinators as C
import Salsa.Party.Looper.Import

runPartyScheduler :: (MonadUnliftIO m, MonadLogger m, MonadReader App m) => m ()
runPartyScheduler = do
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool func pool
  acqOrganiserReminderSource <- runDBHere $ selectSourceRes [] []
  withAcquire acqOrganiserReminderSource $ \scheduleSource -> do
    runConduit $
      scheduleSource
        .| C.mapM (runDBHere . makeScheduleDecision)
        .| C.mapM_ handleScheduleDecision

data ScheduleDecision
  = NextDayTooFarAhead
  | ScheduleAParty (Entity Schedule) [Day] (Maybe ImageId)
  deriving (Show, Eq)

makeScheduleDecision :: MonadUnliftIO m => Entity Schedule -> SqlPersistT m ScheduleDecision
makeScheduleDecision scheduleEntity@(Entity scheduleId_ Schedule {..}) = do
  -- The last-scheduled party of the same scheduler, or nothing if none has been scheduled yet.
  -- We assume that parties are scheduled in chronological order.
  -- TODO we could get rid of this assumption with a nice join.
  mLastParty <- do
    mLastScheduleParty <- selectFirst [SchedulePartySchedule ==. scheduleId_] [Desc SchedulePartyId]
    fmap join $ forM mLastScheduleParty $ \(Entity _ ScheduleParty {..}) -> get schedulePartyParty
  let mLastPartyDay = partyDay <$> mLastParty
  now <- liftIO getCurrentTime
  let today = utctDay now
  let nextDays = nextOccurrences (addDays daysToScheduleAhead today) scheduleRecurrence $ case mLastPartyDay of
        Nothing -> today
        Just day -> day

  case nextDays of
    [] -> pure NextDayTooFarAhead
    _ -> do
      mImageId <- fmap (schedulePosterImage . entityVal) <$> getBy (UniqueSchedulePoster scheduleId_)
      pure $ ScheduleAParty scheduleEntity nextDays mImageId

daysToScheduleAhead :: Integer
daysToScheduleAhead = 45

handleScheduleDecision :: (MonadUnliftIO m, MonadLogger m, MonadReader App m) => ScheduleDecision -> m ()
handleScheduleDecision = \case
  NextDayTooFarAhead -> logDebugN "Not scheduling any parties because the next day would be too far ahead."
  ScheduleAParty (Entity scheduleId_ schedule) nextDays mImageId -> do
    pool <- asks appConnectionPool
    let runDBHere func = runSqlPool func pool
    now <- liftIO getCurrentTime
    runDBHere $
      forM_ nextDays $ \nextDay -> do
        uuid <- nextRandomUUID
        let party = scheduleToPartyOn uuid now nextDay schedule
        partyId_ <- insert party
        insert_ ScheduleParty {schedulePartySchedule = scheduleId_, schedulePartyParty = partyId_, schedulePartyScheduled = now}
        forM_ mImageId $ \imageId_ ->
          insert_
            PartyPoster
              { partyPosterParty = partyId_,
                partyPosterImage = imageId_,
                partyPosterCreated = now,
                partyPosterModified = Nothing
              }

scheduleToPartyOn :: EventUUID -> UTCTime -> Day -> Schedule -> Party
scheduleToPartyOn uuid now day Schedule {..} =
  Party
    { partyUuid = uuid,
      partySlug = mkSlug scheduleTitle,
      partyOrganiser = scheduleOrganiser,
      partyTitle = scheduleTitle,
      partyDescription = scheduleDescription,
      partyDay = day,
      partyStart = scheduleStart,
      partyHomepage = scheduleHomepage,
      partyPrice = schedulePrice,
      partyCancelled = False,
      partyCreated = now,
      partyModified = Nothing,
      partyPlace = schedulePlace
    }
