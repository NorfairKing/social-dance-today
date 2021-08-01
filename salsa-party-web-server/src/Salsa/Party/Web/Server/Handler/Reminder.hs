{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Reminder where

import Salsa.Party.Web.Server.Handler.Import

getUnsubReminderR :: Handler Html
getUnsubReminderR = do
  withNavBar $ do
    setTitleI MsgUnsubReminderTitle
    setDescriptionI MsgUnsubReminderDescription
    $(widgetFile "unsub/reminder")
