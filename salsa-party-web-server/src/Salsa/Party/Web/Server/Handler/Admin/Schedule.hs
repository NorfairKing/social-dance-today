{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Admin.Schedule
  ( getAdminScheduleR,
  )
where

import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import Control.Monad
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Network.URI
import Safe (minimumByMay)
import Salsa.Party.DB.Migration (Location (..), locations)
import Salsa.Party.Email
import Salsa.Party.Looper.ScheduleReminder
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Account.Schedule (getPartiesOfSchedule)
import Salsa.Party.Web.Server.Handler.Admin.Panel (formatAdminDay, formatAdminTime)
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Query (defaultMaximumDistance)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text

getAdminScheduleR :: ScheduleId -> Handler Html
getAdminScheduleR scheduleId = do
  schedule <- runDB $ get404 scheduleId
  mScheduleReminder <- runDB $ getBy $ UniqueScheduleReminderSchedule scheduleId
  organiser <- runDB $ get404 $ scheduleOrganiser schedule
  user <- runDB $ get404 $ organiserUser organiser
  reminderDecision <- runDB $ makeScheduleReminderDecision $ Entity scheduleId schedule
  parties <- runDB $ getPartiesOfSchedule scheduleId

  withNavBar $ do
    token <- genToken
    $(widgetFile "admin/schedule")
