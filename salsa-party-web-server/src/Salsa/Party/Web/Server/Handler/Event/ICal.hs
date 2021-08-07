{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Event.ICal
  ( getEventIcsR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar as ICal

getEventIcsR :: EventUUID -> Handler ICal.VCalendar
getEventIcsR eventUuid = do
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
