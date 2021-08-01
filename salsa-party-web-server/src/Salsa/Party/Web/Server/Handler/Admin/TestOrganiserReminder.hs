{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Admin.TestOrganiserReminder
  ( postAdminTestOrganiserReminderR,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import Salsa.Party.Looper.OrganiserReminder
import Salsa.Party.Web.Server.Handler.Import

postAdminTestOrganiserReminderR :: Handler Html
postAdminTestOrganiserReminderR = do
  mAdminEmailAddress <- getsYesod appAdmin
  forM_ mAdminEmailAddress $ \adminEmailAddress -> do
    logDebugN $ "Sending test organiser reminder to admin: " <> adminEmailAddress
    app <- getYesod
    secret <- nextRandomUUID
    runReaderT (sendOrganiserReminder adminEmailAddress (Just secret)) app
  redirect $ AdminR AdminPanelR
