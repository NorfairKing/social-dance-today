module Salsa.Party.Web.Server.Handler.Event.JSON
  ( getEventExportR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import

getEventExportR :: EventUUID -> Handler TypedContent
getEventExportR eventUuid = do
  mPartyTup <- runDB $ getPartyTupByUuid eventUuid
  case mPartyTup of
    Just (organiserEntity, partyEntity) -> toTypedContent <$> partyPageJSON organiserEntity partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Just externalEventEntity -> toTypedContent <$> externalEventPageJSON externalEventEntity
        Nothing -> notFound
