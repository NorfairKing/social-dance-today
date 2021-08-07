module Salsa.Party.Web.Server.Handler.Event
  ( getEventR,
    getEventIcsR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ICal
import Salsa.Party.Web.Server.Handler.ExternalEvent
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Party

getEventR :: EventUUID -> Handler Html
getEventR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just partyEntity -> partyPage partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> externalEventPage externalEventEntity
        Nothing -> notFound
