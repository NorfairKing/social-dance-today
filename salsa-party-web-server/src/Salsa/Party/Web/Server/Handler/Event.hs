module Salsa.Party.Web.Server.Handler.Event
  ( getEventR,
    getEventIcsR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent
import Salsa.Party.Web.Server.Handler.Event.ICal
import Salsa.Party.Web.Server.Handler.Event.Party
import Salsa.Party.Web.Server.Handler.Import

getEventR :: EventUUID -> Handler TypedContent
getEventR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just partyEntity -> partyPage partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> externalEventPage externalEventEntity
        Nothing -> notFound
