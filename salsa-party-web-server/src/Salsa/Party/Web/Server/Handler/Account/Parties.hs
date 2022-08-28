{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Parties
  ( getAccountPartiesR,
  )
where

import Salsa.Party.Web.Server.Handler.Import

getAccountPartiesR :: Handler Html
getAccountPartiesR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgSubmitPartyErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) -> do
      parties <- runDB $ selectList [PartyOrganiser ==. organiserId] [Desc PartyDay]
      schedules <- runDB $ selectList [ScheduleOrganiser ==. organiserId] [Desc ScheduleCreated]
      timeLocale <- getTimeLocale
      prettyDayFormat <- getPrettyDayFormat
      today <- getClientToday
      withNavBar $(widgetFile "account/parties")
