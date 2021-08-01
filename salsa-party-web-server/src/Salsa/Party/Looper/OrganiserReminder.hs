{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Looper.OrganiserReminder
  ( runOrganiserReminder,
    OrganiserReminderDecision (..),
    makeOrganiserReminderDecision,
    reminderInterval,
    sendOrganiserReminder,
    organiserReminderTextContent,
    organiserReminderHtmlContent,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Conduit.Combinators as C
import Data.Function
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Show.Pretty (ppShow)
import Yesod

-- TODO do this with a join
runOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runOrganiserReminder = do
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool func pool
  acqOrganiserReminderSource <- runDBHere $ selectSourceRes [OrganiserReminderConsent ==. True] []
  withAcquire acqOrganiserReminderSource $ \organiserReminderSource -> do
    runConduit $
      organiserReminderSource
        .| C.mapM (runDBHere . makeOrganiserReminderDecision)
        .| reminderDecisionSink

data OrganiserReminderDecision
  = NoReminderConsent
  | OrganiserNotFound !OrganiserId
  | UserNotFound !UserId
  | UserEmailNotVerified !UserId !Text
  | SentReminderTooRecentlyAlready !UTCTime
  | PartyOrganisedTooRecently !Day -- The scheduled day of the most recent party
  | ShouldSendReminder
      !OrganiserReminderId
      !Text -- Email Address
      !ReminderSecret
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
      let mReminderTooRecent = do
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
              mLastParty <-
                selectFirst
                  [PartyOrganiser ==. organiserReminderOrganiser]
                  [Desc PartyDay]
              let mPartyTooRecent = do
                    Entity _ Party {..} <- mLastParty
                    -- Should send a reminder if the organiser's latest party was more than a week ago or if they have never sent any.
                    --
                    -- TODO we would like to figure out an organisers cadence befor
                    -- we overload them with emails.  For example, an organiser who
                    -- only organises parties monthly doesn't need to be sent
                    -- emails every week.
                    guard $ addUTCTime reminderInterval (max partyCreated (UTCTime partyDay 0)) > now
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
                          else ShouldSendReminder organiserReminderId userEmailAddress organiserReminderSecret
    else pure NoReminderConsent

reminderInterval :: NominalDiffTime
reminderInterval = 7 * nominalDay

reminderDecisionSink :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => ConduitT OrganiserReminderDecision void m ()
reminderDecisionSink = awaitForever $ \case
  NoReminderConsent -> logDebugN "Not sending a reminder because the organiser has revoked consent."
  OrganiserNotFound organiserId -> logWarnN $ T.pack $ unwords ["Organiser not found:", show $ fromSqlKey organiserId]
  UserNotFound userId -> logWarnN $ T.pack $ unwords ["User not found:", show $ fromSqlKey userId]
  UserEmailNotVerified userId emailAddress -> logDebugN $ T.pack $ unwords ["Not sending a reminder because the user's email address has not been validated", show $ fromSqlKey userId, show emailAddress]
  SentReminderTooRecentlyAlready recently -> logDebugN $ T.pack $ unwords ["Not sending a reminder because another reminder has already been sent too recently:", show recently]
  PartyOrganisedTooRecently day -> logDebugN $ T.pack $ unwords ["Not sending a reminder because the organiser has recently organised a party:", show day]
  ShouldSendReminder organiserReminderId emailAddress secret -> lift $ do
    now <- liftIO getCurrentTime
    sendEmails <- asks appSendEmails
    if sendEmails
      then sendOrganiserReminder emailAddress secret
      else logDebugN "Not sending reminder email because sendEmails is off."
    pool <- asks appConnectionPool
    let runDBHere func = runSqlPool func pool
    runDBHere $
      update
        organiserReminderId
        [OrganiserReminderLast =. Just now]

sendOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Text -> ReminderSecret -> m ()
sendOrganiserReminder emailAddress secret = do
  mSendAddress <- asks appSendAddress
  forM_ mSendAddress $ \sendAddress -> do
    logInfoN $ "Sending reminder email to address: " <> emailAddress

    let subject = SES.content "Reminder to submit your parties to social dance today"

    app <- ask
    let urlRender = yesodRender app (fromMaybe "" $ appRoot app)

    let textBody = SES.content $ organiserReminderTextContent urlRender secret
    let htmlBody = SES.content $ organiserReminderHtmlContent urlRender secret

    let body =
          SES.body
            & SES.bText ?~ textBody
            & SES.bHTML ?~ htmlBody

    let message = SES.message subject body

    let destination =
          SES.destination
            & SES.dToAddresses .~ [emailAddress]
    let request = SES.sendEmail sendAddress destination message

    response <- runAWS $ AWS.send request

    case (^. SES.sersResponseStatus) <$> response of
      Right 200 -> logInfoN $ "Succesfully send organiser reminder email to address: " <> emailAddress
      _ -> logErrorN $ T.unlines ["Failed to send organiser reminder email to address: " <> emailAddress, T.pack (ppShow response)]

organiserReminderTextContent :: (Route App -> [(Text, Text)] -> Text) -> ReminderSecret -> Text
organiserReminderTextContent urlRender secret = LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/organiser-reminder.txt") urlRender

organiserReminderHtmlContent :: (Route App -> [(Text, Text)] -> Text) -> ReminderSecret -> Text
organiserReminderHtmlContent urlRender secret = LT.toStrict $ renderHtml $ $(hamletFile "templates/email/organiser-reminder.hamlet") urlRender
