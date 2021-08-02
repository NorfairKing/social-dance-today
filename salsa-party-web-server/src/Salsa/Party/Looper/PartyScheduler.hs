{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Reader
import qualified Data.Conduit.Combinators as C
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import Text.Show.Pretty (pPrint)

runPartyScheduler :: (MonadUnliftIO m, MonadReader App m) => m ()
runPartyScheduler = do
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool func pool
  acqOrganiserReminderSource <- runDBHere $ selectSourceRes [] []
  withAcquire acqOrganiserReminderSource $ \scheduleSource -> do
    runConduit $
      scheduleSource
        .| C.mapM (runDBHere . makeScheduleDecision)
        .| C.mapM_ (liftIO . pPrint)

data ScheduleDecision
  = NextDayTooFarAhead Day
  | ScheduleAParty Day
  deriving (Show, Eq)

makeScheduleDecision :: MonadUnliftIO m => Entity Schedule -> SqlPersistT m ScheduleDecision
makeScheduleDecision (Entity scheduleId Schedule {..}) = do
  -- The last-scheduled party of the same scheduler, or nothing if none has been scheduled yet.
  -- We assume that parties are scheduled in chronological order.
  -- TODO we could get rid of this assumption with a nice join.
  mLastParty <- do
    mLastScheduleParty <- selectFirst [SchedulePartySchedule ==. scheduleId] [Desc SchedulePartyId]
    fmap join $ forM mLastScheduleParty $ \(Entity _ ScheduleParty {..}) -> get schedulePartyParty
  let mLastPartyDay = partyDay <$> mLastParty
  today <- liftIO $ utctDay <$> getCurrentTime
  let nextDay = case mLastPartyDay of
        Nothing -> nextOccurrence scheduleRecurrence today
        Just day -> nextOccurrence scheduleRecurrence day
  pure $
    if addDays daysToScheduleAhead today < nextDay
      then NextDayTooFarAhead nextDay
      else ScheduleAParty nextDay

daysToScheduleAhead :: Integer
daysToScheduleAhead = 45
