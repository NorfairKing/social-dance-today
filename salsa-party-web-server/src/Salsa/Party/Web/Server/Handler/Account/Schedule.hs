{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account.Schedule
  ( getAccountSchedulesR,
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Poster

getAccountSchedulesR :: Handler Html
getAccountSchedulesR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgSubmitPartyErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId organiser) -> do
      schedules <- runDB $ selectList [ScheduleOrganiser ==. organiserId] [Asc ScheduleId]
      token <- genToken
      timeLocale <- getTimeLocale
      prettyDayFormat <- getPrettyDayFormat
      today <- liftIO $ utctDay <$> getCurrentTime
      withNavBar $(widgetFile "account/schedules")
