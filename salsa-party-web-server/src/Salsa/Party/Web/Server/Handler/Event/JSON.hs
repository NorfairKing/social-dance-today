module Salsa.Party.Web.Server.Handler.Event.JSON
  ( getEventExportR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Import

getEventExportR :: EventUUID -> Handler TypedContent
getEventExportR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just partyEntity -> toTypedContent <$> partyPageJSON partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> toTypedContent <$> externalEventPageJSON externalEventEntity
        Nothing -> notFound
