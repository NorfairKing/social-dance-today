{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Looper.OrganiserReminder (runOrganiserReminder, sendOrganiserReminder) where

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

-- TODO do this with a RightOuterJoin of Organiser and OrganiserReminder
runOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runOrganiserReminder = do
  sendEmails <- asks appSendEmails
  logDebugN "Not running the organiser reminder looper checks because sendEmails is off."
  when sendEmails $ do
    pool <- asks appConnectionPool
    let runDBHere func = runSqlPool func pool
    acqOrganiserReminderSource <- runDBHere $ selectSourceRes [OrganiserReminderConsent ==. True] []
    withAcquire acqOrganiserReminderSource $ \organiserReminderSource -> do
      runConduit $ organiserReminderSource .| C.mapM_ (runDBHere . checkToSendOrganiserReminder)

checkToSendOrganiserReminder :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Entity OrganiserReminder -> SqlPersistT m ()
checkToSendOrganiserReminder (Entity organiserReminderId OrganiserReminder {..}) = do
  logDebugN $
    T.pack $
      unwords
        [ "Checking whether to send an organiser reminder to ",
          show (fromSqlKey organiserReminderOrganiser)
        ]
  when organiserReminderConsent $ do
    mOrganiser <- get organiserReminderOrganiser
    forM_ mOrganiser $ \organiser -> do
      mUser <- get $ organiserUser organiser
      forM_ mUser $ \User {..} ->
        if isJust userVerificationKey
          then logDebugN $ "Not sending a reminder to unverified user: " <> userEmailAddress
          else do
            now <- liftIO getCurrentTime
            let readyToSendReminder = case organiserReminderLast of
                  Nothing -> True
                  Just lastReminder -> addUTCTime reminderInterval lastReminder < now
            if readyToSendReminder
              then do
                logDebugN "Ready to send a reminder in terms of how long it's been since the last."
                mLastParty <-
                  selectFirst
                    [PartyOrganiser ==. organiserReminderOrganiser]
                    [Desc PartyCreated]
                let shouldSendReminder = case mLastParty of
                      -- No parties yet, definitely don't send any reminders yet.
                      Nothing -> False
                      -- Should send a reminder if the organiser's latest party was more than a week ago.
                      --
                      -- TODO we would like to figure out an organisers cadence befor
                      -- we overload them with emails.  For example, an organiser who
                      -- only organises parties monthly doesn't need to be sent
                      -- emails every week.
                      Just (Entity _ Party {..}) ->
                        addUTCTime reminderInterval (max partyCreated (UTCTime partyDay 0)) < now

                if shouldSendReminder
                  then do
                    lift $ sendOrganiserReminder userEmailAddress
                    void $
                      update
                        organiserReminderId
                        [OrganiserReminderLast =. Just now]
                  else logDebugN "Not sending a reminder email because the organiser either hasn't organised anything yet or has organised something recently."
              else logDebugN "Not sending a reminder because another one has been sent too recently."

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
