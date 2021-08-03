{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.PartyScheduler
  ( runPartyScheduler,
    ScheduleDecision (..),
    makeScheduleDecision,
    daysToScheduleAhead,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation

runPartyScheduler :: (MonadUnliftIO m, MonadLogger m, MonadReader App m) => m ()
runPartyScheduler = do
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool func pool
  acqOrganiserReminderSource <- runDBHere $ selectSourceRes [] []
  withAcquire acqOrganiserReminderSource $ \scheduleSource -> do
    runConduit $
      scheduleSource
        .| C.mapM (runDBHere . makeScheduleDecision)
        .| scheduleDecisionSink

data ScheduleDecision
  = NextDayTooFarAhead Day
  | ScheduleAParty ScheduleId Party (Maybe ImageId)
  deriving (Show, Eq)

makeScheduleDecision :: MonadUnliftIO m => Entity Schedule -> SqlPersistT m ScheduleDecision
makeScheduleDecision (Entity scheduleId_ schedule@Schedule {..}) = do
  -- The last-scheduled party of the same scheduler, or nothing if none has been scheduled yet.
  -- We assume that parties are scheduled in chronological order.
  -- TODO we could get rid of this assumption with a nice join.
  mLastParty <- do
    mLastScheduleParty <- selectFirst [SchedulePartySchedule ==. scheduleId_] [Desc SchedulePartyId]
    fmap join $ forM mLastScheduleParty $ \(Entity _ ScheduleParty {..}) -> get schedulePartyParty
  let mLastPartyDay = partyDay <$> mLastParty
  now <- liftIO getCurrentTime
  let today = utctDay now
  let nextDay = case mLastPartyDay of
        Nothing -> nextOccurrence scheduleRecurrence today
        Just day -> nextOccurrence scheduleRecurrence day
  if addDays daysToScheduleAhead today < nextDay
    then pure $ NextDayTooFarAhead nextDay
    else do
      uuid <- nextRandomUUID
      mImageId <- fmap (schedulePosterImage . entityVal) <$> getBy (UniqueSchedulePoster scheduleId_)
      pure $ ScheduleAParty scheduleId_ (scheduleToPartyOn uuid now nextDay schedule) mImageId

scheduleToPartyOn :: EventUUID -> UTCTime -> Day -> Schedule -> Party
scheduleToPartyOn uuid now day Schedule {..} =
  Party
    { partyUuid = uuid,
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

daysToScheduleAhead :: Integer
daysToScheduleAhead = 45

scheduleDecisionSink :: (MonadUnliftIO m, MonadLogger m, MonadReader App m) => ConduitT ScheduleDecision Void m ()
scheduleDecisionSink = awaitForever $ \case
  NextDayTooFarAhead day -> logDebugN $ T.pack $ "Not scheduling a party because the next day would be too far ahead:" <> show day
  ScheduleAParty scheduleId_ party mImageId -> lift $ do
    pool <- asks appConnectionPool
    let runDBHere func = runSqlPool func pool
    now <- liftIO getCurrentTime
    runDBHere $ do
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
