{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Looper.OrganiserReminder
  ( runOrganiserReminder,
    OrganiserReminderDecision (..),
    makeOrganiserReminderDecision,
    reminderInterval,
    sendOrganiserReminder,
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
import Text.Show.Pretty (pPrint, ppShow)
import Yesod

-- TODO do this with a join
runOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runOrganiserReminder = do
  sendEmails <- asks appSendEmails
  logDebugN "Not running the organiser reminder looper checks because sendEmails is off."
  when sendEmails $ do
    pool <- asks appConnectionPool
    let runDBHere func = runSqlPool func pool
    acqOrganiserReminderSource <- runDBHere $ selectSourceRes [OrganiserReminderConsent ==. True] []
    withAcquire acqOrganiserReminderSource $ \organiserReminderSource -> do
      runConduit $
        organiserReminderSource
          .| C.mapM (runDBHere . makeOrganiserReminderDecision)
          .| C.mapM_ (liftIO . pPrint)

data OrganiserReminderDecision
  = NoReminderConsent
  | OrganiserNotFound !OrganiserId
  | UserNotFound !UserId
  | UserEmailNotVerified !UserId !Text
  | SentReminderTooRecentlyAlready !UTCTime
  | PartyOrganisedTooRecently !Day -- The scheduled day of the most recent party
  | ShouldSendReminder !OrganiserReminderId !Text -- Email Address
  deriving (Show, Eq)

-- Check whether to send an organiser reminder, return the email address to send it to if we should.
makeOrganiserReminderDecision :: (MonadUnliftIO m, MonadLoggerIO m) => Entity OrganiserReminder -> SqlPersistT m OrganiserReminderDecision
makeOrganiserReminderDecision (Entity organiserReminderId OrganiserReminder {..}) = do
  logDebugN $
    T.pack $
      unwords
        [ "Checking whether to send an organiser reminder to ",
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
          logDebugN "Ready to send a reminder in terms of how long it's been since the last."
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
                          else ShouldSendReminder organiserReminderId userEmailAddress
    else pure NoReminderConsent

reminderInterval :: NominalDiffTime
reminderInterval = 7 * nominalDay

sendOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Text -> m ()
sendOrganiserReminder emailAddress = do
  mSendAddress <- asks appSendAddress
  forM_ mSendAddress $ \sendAddress -> do
    logInfoN $ "Sending reminder email to address: " <> emailAddress

    let subject = SES.content "Reminder to submit your parties to social dance today"

    app <- ask
    let urlRender = yesodRender app (fromMaybe "" $ appRoot app)

    let textBody = SES.content $ LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/organiser-reminder.txt") urlRender

    let htmlBody = SES.content $ LT.toStrict $ renderHtml $ $(hamletFile "templates/email/organiser-reminder.hamlet") urlRender

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

-- void $
--   update
--     organiserReminderId
--     [OrganiserReminderLast =. Just now]
