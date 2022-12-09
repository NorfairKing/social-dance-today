module Salsa.Party.Web.Server.Handler.Event.Export
  ( getEventExportR,
  )
where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Export
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
import Salsa.Party.Web.Server.Handler.Event.Party.Export
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import
import Yesod.Core.Types

getEventExportR :: EventUUID -> Handler TypedContent
getEventExportR eventUuid = do
  mPartyTup <- runDB $ getPartyTupByUuid eventUuid
  case mPartyTup of
    Just (organiser, Entity _ party) -> toTypedContent . JSONResponse <$> exportParty organiser party
    Nothing -> do
      mExternalEvent <- runDB $ getExternalEventTupByUuid eventUuid
      case mExternalEvent of
        Just (externalEvent, place) -> toTypedContent . JSONResponse <$> exportExternalEvent externalEvent place
        Nothing -> notFound
