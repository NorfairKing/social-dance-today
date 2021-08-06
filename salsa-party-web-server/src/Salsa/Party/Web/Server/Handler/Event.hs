{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Event
  ( getPartyR,
    getPartyEventIcsR,
  )
where

import Salsa.Party.Web.Server.Handler.ExternalEvent
import Salsa.Party.Web.Server.Handler.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Party
import qualified Text.ICalendar as ICal

getPartyR :: EventUUID -> Handler Html
getPartyR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just partyEntity -> partyPage partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> externalEventPage externalEventEntity
        Nothing -> notFound

getPartyEventIcsR :: EventUUID -> Handler ICal.VCalendar
getPartyEventIcsR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just (Entity _ party) -> do
      place@Place {..} <- runDB $ get404 $ partyPlace party
      renderUrl <- getUrlRender
      pure $ partyCalendar renderUrl party place
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just (Entity _ externalEvent) -> do
          place@Place {..} <- runDB $ get404 $ externalEventPlace externalEvent
          renderUrl <- getUrlRender
          pure $ externalEventCalendar renderUrl externalEvent place
        Nothing -> notFound
