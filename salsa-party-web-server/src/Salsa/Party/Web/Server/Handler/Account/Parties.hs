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

import Control.Monad
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Handler.Import

getAccountPartiesR :: Handler Html
getAccountPartiesR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgSubmitPartyErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId organiser) -> do
      parties <- runDB $ selectList [PartyOrganiser ==. organiserId] [Desc PartyDay]
      schedules <- runDB $ selectList [ScheduleOrganiser ==. organiserId] [Desc ScheduleCreated]
      token <- genToken
      timeLocale <- getTimeLocale
      prettyDayFormat <- getPrettyDayFormat
      today <- liftIO $ utctDay <$> getCurrentTime
      withNavBar $(widgetFile "account/parties")
