{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Admin.TestOrganiserReminder
  ( postAdminTestOrganiserReminderR,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Salsa.Party.Looper.OrganiserReminder
import Salsa.Party.Web.Server.Handler.Import

postAdminTestOrganiserReminderR :: Handler Html
postAdminTestOrganiserReminderR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> invalidArgs ["Admin has no organiser profile."]
    Just (Entity organiserId Organiser {..}) -> do
      mOrganiserReminder <- runDB $ getBy $ UniqueOrganiserReminderOrganiser organiserId
      case mOrganiserReminder of
        Nothing -> invalidArgs ["Admin has not consented to organiser reminders."]
        Just (Entity _ OrganiserReminder {..}) -> do
          mAdminEmailAddress <- getsYesod appAdmin
          forM_ mAdminEmailAddress $ \adminEmailAddress -> do
            logDebugN $ T.pack $ "Sending test organiser reminder to admin: " <> show adminEmailAddress
            app <- getYesod
            runReaderT (sendOrganiserReminder adminEmailAddress organiserReminderSecret) app
          redirect $ AdminR AdminPanelR
