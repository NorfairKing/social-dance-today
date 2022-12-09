module Salsa.Party.Web.Server.Handler.Event.ICal
  ( getEventIcsR,
  )
where

import qualified ICal.Component as ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import

getEventIcsR :: EventUUID -> Handler ICal.Calendar
getEventIcsR eventUuid = do
  mPartyTup <- runDB $ getPartyTupByUuid eventUuid
  case mPartyTup of
    Just (organiser, Entity _ party) -> partyPageICal organiser party
    Nothing -> do
      mExternalEvent <- runDB $ getExternalEventTupByUuid eventUuid
      case mExternalEvent of
        Just (externalEvent, place) -> externalEventPageICal externalEvent place
        Nothing -> notFound
