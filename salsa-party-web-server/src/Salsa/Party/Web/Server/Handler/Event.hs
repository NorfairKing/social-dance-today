module Salsa.Party.Web.Server.Handler.Event
  ( getEventR,
    getEventIcsR,
    getEventExportR,
    getPartySlugR,
    getExternalEventSlugR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.Export
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
import Salsa.Party.Web.Server.Handler.Event.ICal
import Salsa.Party.Web.Server.Handler.Event.Party
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import

getEventR :: EventUUID -> Handler TypedContent
getEventR eventUuid = do
  mPartyTup <- runDB $ getPartyTupByUuid eventUuid
  case mPartyTup of
    Just partyTup@(Entity _ organiser, Entity _ party) -> case partySlugRoute organiser party of
      Nothing -> partyPage partyTup
      Just route -> redirect route
    Nothing -> do
      mExternalEventTup <- runDB $ getExternalEventTupByUuid eventUuid
      case mExternalEventTup of
        Just (externalEventEntity@(Entity _ externalEvent), placeEntity, mCASKey) -> case externalEventSlugRoute externalEvent of
          Nothing -> externalEventPage externalEventEntity placeEntity mCASKey
          Just route -> redirect route
        Nothing -> notFound

getPartySlugR :: OrganiserSlug -> EventSlug -> Day -> Handler TypedContent
getPartySlugR organiserSlug_ partySlug_ day = do
  mPartyTup <- runDB $ getPartyTupBySlug organiserSlug_ partySlug_ day
  case mPartyTup of
    Nothing -> notFound
    Just partyTup -> partyPage partyTup

getExternalEventSlugR :: EventSlug -> Day -> Handler TypedContent
getExternalEventSlugR externalEventSlug_ day = do
  mExternalEventTup <- runDB $ getExternalEventTupBySlug externalEventSlug_ day
  case mExternalEventTup of
    Nothing -> notFound
    Just (externalEventEntity, placeEntity, mCASKey) -> externalEventPage externalEventEntity placeEntity mCASKey
