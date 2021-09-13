module Salsa.Party.Web.Server.Handler.Event
  ( getEventR,
    getEventIcsR,
    getEventExportR,
    getPartySlugR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent
import Salsa.Party.Web.Server.Handler.Event.ICal
import Salsa.Party.Web.Server.Handler.Event.JSON
import Salsa.Party.Web.Server.Handler.Event.Party
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import

getEventR :: EventUUID -> Handler TypedContent
getEventR eventUuid = do
  mPartyTup <- runDB $ getPartyTupByUuid eventUuid
  case mPartyTup of
    Just partyTup@(Entity _ organiser, Entity _ party) -> case (,) <$> organiserSlug organiser <*> partySlug party of
      Nothing -> partyPage partyTup
      Just (organiserSlug_, partySlug_) -> redirect $ PartySlugR organiserSlug_ partySlug_ (partyDay party)
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> externalEventPage externalEventEntity
        Nothing -> notFound
