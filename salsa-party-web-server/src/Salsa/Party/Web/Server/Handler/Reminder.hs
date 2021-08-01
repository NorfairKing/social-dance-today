{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Reminder where

import Salsa.Party.Web.Server.Handler.Import

getUnsubReminderR :: ReminderSecret -> Handler Html
getUnsubReminderR secret = do
  mOrganiserReminder <- runDB $ getBy $ UniqueOrganiserReminderSecret $ Just secret
  case mOrganiserReminder of
    Nothing -> notFound
    Just (Entity organiserReminderId OrganiserReminder {..}) -> do
      runDB $ do
        update organiserReminderId [OrganiserReminderConsent =. False]
      withNavBar $ do
        setTitleI MsgUnsubReminderTitle
        setDescriptionI MsgUnsubReminderDescription
        $(widgetFile "unsub/reminder")
