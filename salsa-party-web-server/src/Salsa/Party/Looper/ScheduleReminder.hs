{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Looper.ScheduleReminder
  ( runScheduleReminder,
    ScheduleReminderDecision (..),
    makeScheduleReminderDecision,
    scheduleReminderGraceTime,
  )
where

import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Validity
import GHC.Generics (Generic)
import Salsa.Party.Email
import Salsa.Party.Looper.Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Yesod

-- TODO do this with a join
runScheduleReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runScheduleReminder = do
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool (retryOnBusy func) pool
  runConduit $
    transPipe runDBHere (streamEntities [] ScheduleId (PageSize 64) Ascend (Range Nothing Nothing))
      .| C.mapM (runDBHere . makeScheduleReminderDecision)
      .| scheduleReminderDecisionSink

data ScheduleReminderDecision
  = NotDueForVerificationUntil !UTCTime
  | ScheduleOrganiserNotFound !OrganiserId
  | ScheduleUserNotFound !UserId
  | SentScheduleReminderTooRecentlyAlready !UTCTime
  | ShouldSendScheduleReminder !EmailAddress
  deriving (Show, Eq, Generic)

instance Validity ScheduleReminderDecision

makeScheduleReminderDecision :: (MonadUnliftIO m, MonadLoggerIO m) => Entity Schedule -> SqlPersistT m ScheduleReminderDecision
makeScheduleReminderDecision (Entity scheduleId Schedule {..}) = do
  logDebugN $
    T.pack $
      unwords
        [ "Checking whether to send an schedule reminder about schedule",
          show (fromSqlKey scheduleId)
        ]

  mOrganiser <- get scheduleOrganiser
  case mOrganiser of
    Nothing -> pure $ ScheduleOrganiserNotFound scheduleOrganiser
    Just Organiser {..} -> do
      mUser <- get organiserUser
      case mUser of
        Nothing -> pure $ ScheduleUserNotFound organiserUser
        Just User {..} -> do
          mScheduleReminder <- getBy (UniqueScheduleReminderSchedule scheduleId)
          let (mReminded, mVerified) = case mScheduleReminder of
                Nothing -> (Nothing, Nothing)
                Just (Entity _ ScheduleReminder {..}) -> (scheduleReminderReminded, scheduleReminderVerified)
          liftIO $ print (mReminded, mVerified)

          pure $ ShouldSendScheduleReminder userEmailAddress

scheduleReminderGraceTime :: NominalDiffTime
scheduleReminderGraceTime = 30 * nominalDay

scheduleReminderDecisionSink :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => ConduitT ScheduleReminderDecision void m ()
scheduleReminderDecisionSink = undefined

sendScheduleReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => EmailAddress -> m ()
sendScheduleReminder = undefined
