{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Looper.OrganiserReminder
  ( runOrganiserReminder,
    OrganiserReminderDecision (..),
    makeOrganiserReminderDecision,
    gracePeriodAfterRegistration,
    gracePeriodAfterParty,
    reminderInterval,
    sendOrganiserReminder,
    organiserReminderTextContent,
    organiserReminderHtmlContent,
  )
where

import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Salsa.Party.Email
import Salsa.Party.Looper.Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Yesod

-- TODO do this with a join
runOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runOrganiserReminder = do
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool (retryOnBusy func) pool
  acqOrganiserReminderSource <- runDBHere $ selectSourceRes [OrganiserReminderConsent ==. True] []
  withAcquire acqOrganiserReminderSource $ \organiserReminderSource -> do
    runConduit $
      organiserReminderSource
        .| C.mapM (runDBHere . makeOrganiserReminderDecision)
        .| reminderDecisionSink

data OrganiserReminderDecision
  = NoReminderConsent
  | SentReminderTooRecentlyAlready !UTCTime
  | OrganiserNotFound !OrganiserId
  | PartyOrganisedTooRecently !Day -- The scheduled day of the most recent party
  | UserNotFound !UserId
  | UserEmailNotVerified !UserId !EmailAddress
  | UserTooNew !UserId !UTCTime
  | ShouldSendReminder !OrganiserReminderId !EmailAddress !ReminderSecret
  deriving (Show, Eq)

-- Check whether to send an organiser reminder, return the email address to send it to if we should.
makeOrganiserReminderDecision :: (MonadUnliftIO m, MonadLoggerIO m) => Entity OrganiserReminder -> SqlPersistT m OrganiserReminderDecision
makeOrganiserReminderDecision (Entity organiserReminderId OrganiserReminder {..}) = do
  logDebugN $
    T.pack $
      unwords
        [ "Checking whether to send an organiser reminder to organiser",
          show (fromSqlKey organiserReminderOrganiser)
        ]
  if organiserReminderConsent
    then do
      now <- liftIO getCurrentTime
      let -- The time of the last reminder that was too recent
          -- Nothing if it wasn't too recent or there hasn't been a reminder
          -- yet.
          mReminderTooRecent :: Maybe UTCTime
          mReminderTooRecent = do
            lastReminder <- organiserReminderLast
            guard $ addUTCTime reminderInterval lastReminder > now
            pure lastReminder
      case mReminderTooRecent of
        Just tooRecent -> pure $ SentReminderTooRecentlyAlready tooRecent
        Nothing -> do
          mOrganiser <- get organiserReminderOrganiser
          case mOrganiser of
            Nothing -> pure $ OrganiserNotFound organiserReminderOrganiser
            Just Organiser {..} -> do
              -- We assume that parties are created before they are scheduled
              mLastParty <-
                selectFirst
                  [PartyOrganiser ==. organiserReminderOrganiser]
                  [Desc PartyDay]
              let -- Day of the most recent party that was too recent
                  -- Nothing if there was no party too recently.
                  mPartyTooRecent :: Maybe Day
                  mPartyTooRecent = do
                    Entity _ Party {..} <- mLastParty
                    -- Should send a reminder if the organiser's latest party was more than a week ago or if they have never sent any.
                    --
                    -- TODO we would like to figure out an organisers cadence
                    -- before we overload them with emails.  For example, an
                    -- organiser who only organises parties monthly doesn't
                    -- need to be sent emails every week.
                    --
                    -- EDIT: Alternatively, when we get recurrence, we can just
                    -- not do that and just send weekly reminders, that's
                    -- probably fine.
                    guard $ addDays gracePeriodAfterParty partyDay > utctDay now
                    pure partyDay
              case mPartyTooRecent of
                Just partyDay -> pure $ PartyOrganisedTooRecently partyDay
                Nothing -> do
                  mUser <- get organiserUser
                  case mUser of
                    Nothing -> pure $ UserNotFound organiserUser
                    Just User {..} ->
                      pure $
                        if isJust userVerificationKey
                          then UserEmailNotVerified organiserUser userEmailAddress
                          else
                            if addUTCTime gracePeriodAfterRegistration userCreated > now
                              then UserTooNew organiserUser userCreated
                              else ShouldSendReminder organiserReminderId userEmailAddress organiserReminderSecret
    else pure NoReminderConsent

-- The amount of time we wait after a user has registered (but not submitted
-- any parties) before we start sending reminders.
gracePeriodAfterRegistration :: NominalDiffTime
gracePeriodAfterRegistration = 3 * nominalDay

-- The amount of time we wait after the last party to send a reminder, in days.
gracePeriodAfterParty :: Integer
gracePeriodAfterParty = 3

-- How often (at most) we send reminders.
reminderInterval :: NominalDiffTime
reminderInterval = 7 * nominalDay

reminderDecisionSink :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => ConduitT OrganiserReminderDecision void m ()
reminderDecisionSink = awaitForever $ \case
  NoReminderConsent -> logDebugN "Not sending a reminder because the organiser has revoked consent."
  SentReminderTooRecentlyAlready recently ->
    logDebugN $
      T.pack $
        unwords
          [ "Not sending a reminder because another reminder has already been sent too recently:",
            show recently
          ]
  OrganiserNotFound organiserId ->
    logWarnN $
      T.pack $
        unwords
          [ "Organiser not found:",
            show $ fromSqlKey organiserId
          ]
  PartyOrganisedTooRecently day ->
    logDebugN $
      T.pack $
        unwords
          [ "Not sending a reminder because the organiser has recently organised a party:",
            show day
          ]
  UserNotFound userId ->
    logWarnN $
      T.pack $
        unwords
          [ "User not found:",
            show $ fromSqlKey userId
          ]
  UserEmailNotVerified userId emailAddress ->
    logDebugN $
      T.pack $
        unwords
          [ "Not sending a reminder because the user's email address has not been validated",
            show $ fromSqlKey userId,
            show emailAddress
          ]
  UserTooNew userId created ->
    logDebugN $
      T.pack $
        unwords
          [ "Not sending a reminder because the user's is too new (the gracePeriod hasn't passed yet):",
            show $ fromSqlKey userId,
            show created
          ]
  ShouldSendReminder organiserReminderId emailAddress secret -> lift $ do
    now <- liftIO getCurrentTime
    sendOrganiserReminder emailAddress secret
    pool <- asks appConnectionPool
    let runDBHere func = runSqlPool (retryOnBusy func) pool
    runDBHere $
      update
        organiserReminderId
        [OrganiserReminderLast =. Just now]

sendOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => EmailAddress -> ReminderSecret -> m ()
sendOrganiserReminder emailAddress secret = do
  logInfoN $ T.pack $ unwords ["Sending reminder email to address:", show emailAddress]

  let subject = SES.newContent "Reminder to submit your parties to social dance today"

  app <- ask
  let urlRender = yesodRender app (fromMaybe "" $ appRoot app)

  let textBody = SES.newContent $ organiserReminderTextContent urlRender secret
  let htmlBody = SES.newContent $ organiserReminderHtmlContent urlRender secret

  let body = SES.newBody {SES.html = Just htmlBody, SES.text = Just textBody}

  let message = SES.newMessage subject body

  let destination = SES.newDestination {SES.toAddresses = Just [emailAddressText emailAddress]}

  sendEmailResult <- sendEmailFromNoReply app destination message
  case sendEmailResult of
    NoEmailSent -> logWarnN "No organiser reminder email sent."
    EmailSentSuccesfully -> logInfoN $ T.pack $ unwords ["Succesfully send organiser reminder email to address:", show emailAddress]
    ErrorWhileSendingEmail _ -> logErrorN $ T.pack $ unwords ["Failed to send organiser reminder email to address:", show emailAddress]

organiserReminderTextContent :: (Route App -> [(Text, Text)] -> Text) -> ReminderSecret -> Text
organiserReminderTextContent urlRender secret = LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/organiser-reminder.txt") urlRender

organiserReminderHtmlContent :: (Route App -> [(Text, Text)] -> Text) -> ReminderSecret -> Text
organiserReminderHtmlContent urlRender secret = LT.toStrict $ renderHtml $ $(hamletFile "templates/email/organiser-reminder.hamlet") urlRender
