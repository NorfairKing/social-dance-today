{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Reminder where

import Salsa.Party.Web.Server.Handler.Import

getUnsubReminderR :: OrganiserReminderSecret -> Handler Html
getUnsubReminderR secret = do
  mOrganiserReminder <- runDB $ getBy $ UniqueOrganiserReminderSecret secret
  case mOrganiserReminder of
    Nothing -> notFound
    Just (Entity organiserReminderId _) -> do
      runDB $ do
        update organiserReminderId [OrganiserReminderConsent =. False]
      withNavBar $ do
        setTitleI MsgUnsubReminderTitle
        setDescriptionIdempI MsgUnsubReminderDescription
        $(widgetFile "unsub/reminder")
