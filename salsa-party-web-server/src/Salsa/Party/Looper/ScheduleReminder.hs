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
    scheduleReminderGraceTimeToBeReminded,
    scheduleReminderGraceTimeToVerify,
    scheduleReminderTextContent,
    scheduleReminderHtmlContent,
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

-- | Decide whether to send a schedule reminder email
--
-- To avoid that organisers forget to update/cancel/delete schedules, we send
-- them a reminder email.
--
-- We want to make sure that every schedule has been verified within the last 3
-- months.
-- (See scheduleReminderGraceTimeToVerify.)
--
-- A schedule is considered verified if any of these timestamps are recent enough:
--   * Created
--   * Modified
--   * Verified
-- Let the maximum of these three be called 'latestTime'
--
-- In order to help organisers keep schedules verified, we send them a reminder
-- no sooner than 2 months after the 'latestTime'.
-- (See scheduleReminderGraceTimeToBeReminded.)
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

          let latestModification = fromMaybe scheduleCreated scheduleModified
          let latestVerification = fromMaybe scheduleCreated mVerified
          let latestTime = max latestModification latestVerification

          -- When we should send the next reminder the soonest.
          let nextReminderTime = addUTCTime scheduleReminderGraceTimeToBeReminded latestTime

          now <- liftIO getCurrentTime

          pure $
            if nextReminderTime < now
              then case mReminded of
                Nothing -> ShouldSendScheduleReminder scheduleId userEmailAddress
                Just reminded -> SentScheduleReminderTooRecentlyAlready reminded
              else NotDueForReminderUntil nextReminderTime

data ScheduleReminderDecision
  = -- | The schedule has been recently created/modified/verified.
    NotDueForReminderUntil !UTCTime
  | ScheduleOrganiserNotFound !OrganiserId
  | ScheduleUserNotFound !UserId
  | -- | The schedule is due for a reminder and not recently verified, but we've already sent the reminder.
    SentScheduleReminderTooRecentlyAlready !UTCTime
  | -- | We should send a reminder now because:
    -- * The schedule has not been created/modified/verified in 'scheduleReminderGraceTimeToBeReminded'
    -- * We haven't sent a reminder yet.
    ShouldSendScheduleReminder !ScheduleId !EmailAddress
  deriving (Show, Eq, Generic)

instance Validity ScheduleReminderDecision

-- | How long after creation or verification we send a reminder
scheduleReminderGraceTimeToBeReminded :: NominalDiffTime
scheduleReminderGraceTimeToBeReminded = 2 * 30 * nominalDay

-- | How long after the remind we mark a schedule as 'possibly out of date'
--
-- Must be more than 'scheduleReminderGraceTimeToBeReminded'
scheduleReminderGraceTimeToVerify :: NominalDiffTime
scheduleReminderGraceTimeToVerify = 3 * 30 * nominalDay

scheduleReminderDecisionSink :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => ConduitT ScheduleReminderDecision void m ()
scheduleReminderDecisionSink = C.mapM_ $ \case
  NotDueForReminderUntil ut ->
    logDebugN $
      T.pack $
        unwords
          [ "Not sending a schedule reminder because the schedule is not due for a reminder until:",
            show ut
          ]
  ScheduleOrganiserNotFound organiserId ->
    logErrorN $
      T.pack $
        unwords
          [ "Organiser not found:",
            show $ fromSqlKey organiserId
          ]
  ScheduleUserNotFound userId ->
    logErrorN $
      T.pack $
        unwords
          [ "User not found:",
            show $ fromSqlKey userId
          ]
  SentScheduleReminderTooRecentlyAlready ut ->
    logDebugN $
      T.pack $
        unwords
          [ "Not sending a schedule reminder because we've already recently sent a reminder:",
            show ut
          ]
  ShouldSendScheduleReminder scheduleId emailAddress -> do
    logInfoN $
      T.pack $
        unwords
          [ "Sending schedule reminder email to address:",
            show emailAddress
          ]
    now <- liftIO getCurrentTime
    sendEmailResult <- sendScheduleReminder emailAddress
    case sendEmailResult of
      NoEmailSent -> logWarnN "No schedule reminder email sent."
      ErrorWhileSendingEmail _ -> logErrorN $ T.pack $ unwords ["Failed to send schedule reminder email to address:", show emailAddress]
      EmailSentSuccesfully -> do
        logInfoN $ T.pack $ unwords ["Succesfully send schedule reminder email to address:", show emailAddress, "about schedule", show (fromSqlKey scheduleId)]
        pool <- asks appConnectionPool
        let runDBHere func = runSqlPool (retryOnBusy func) pool
        void $
          runDBHere $
            upsertBy
              (UniqueScheduleReminderSchedule scheduleId)
              ( ScheduleReminder
                  { scheduleReminderSchedule = scheduleId,
                    scheduleReminderReminded = Just now,
                    scheduleReminderVerified = Nothing
                  }
              )
              [ScheduleReminderReminded =. Just now]

sendScheduleReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => EmailAddress -> m SendEmailResult
sendScheduleReminder emailAddress = do
  let subject = SES.newContent "Action required: Verify your recurring party on Social Dance Today."

  app <- ask
  let urlRender = yesodRender app (fromMaybe "" $ appRoot app)

  let textBody = SES.newContent $ scheduleReminderTextContent urlRender
  let htmlBody = SES.newContent $ scheduleReminderHtmlContent urlRender

  let body = SES.newBody {SES.html = Just htmlBody, SES.text = Just textBody}

  let message = SES.newMessage subject body

  let destination = SES.newDestination {SES.toAddresses = Just [emailAddressText emailAddress]}

  sendEmailFromNoReply app destination message

scheduleReminderTextContent :: (Route App -> [(Text, Text)] -> Text) -> Text
scheduleReminderTextContent urlRender = LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/schedule-reminder.txt") urlRender

scheduleReminderHtmlContent :: (Route App -> [(Text, Text)] -> Text) -> Text
scheduleReminderHtmlContent urlRender = LT.toStrict $ renderHtml $ $(hamletFile "templates/email/schedule-reminder.hamlet") urlRender
