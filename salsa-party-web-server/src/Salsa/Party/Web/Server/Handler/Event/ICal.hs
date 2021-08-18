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
    Just partyEntity -> partyPageICal partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> externalEventPageICal externalEventEntity
        Nothing -> notFound
