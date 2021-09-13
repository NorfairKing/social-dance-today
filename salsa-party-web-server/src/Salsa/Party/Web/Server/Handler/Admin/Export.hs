{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Admin.Export (getAdminExportDayR) where

import Codec.Archive.Zip as Zip
import Data.Aeson as JSON
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID.Typed as Typed
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Import

getAdminExportDayR :: Day -> Handler TypedContent
getAdminExportDayR day = do
  partyExports <- do
    partyTups <- runDB $
      E.select $
        E.from $ \(organiser `E.InnerJoin` party) -> do
          E.on $ party E.^. PartyOrganiser E.==. organiser E.^. OrganiserId
          pure (organiser, party)
    mapM (uncurry exportParty) partyTups

  externalEvents <- runDB $ selectList [ExternalEventDay ==. day] []
  externalEventExports <- mapM exportExternalEvent externalEvents
  let jsonEntry :: ToJSON a => FilePath -> a -> Zip.Entry
      jsonEntry fp a = Zip.toEntry fp 0 (JSON.encode a)
      uuidPath :: Typed.UUID a -> FilePath
      uuidPath u =
        let s = uuidString u
         in s <> "/" <> s <> ".json"
      partyEntry :: PartyExport -> Zip.Entry
      partyEntry pe = jsonEntry (uuidPath (partyExportUuid pe)) pe
      externalEventEntry :: ExternalEventExport -> Zip.Entry
      externalEventEntry eee = jsonEntry (uuidPath (externalEventExportUuid eee)) eee
  let addEntriesToArchive = foldl' (flip addEntryToArchive)
  let archive = addEntriesToArchive emptyArchive $ map partyEntry partyExports ++ map externalEventEntry externalEventExports
  addHeader "Content-Disposition" $ T.concat ["attachment; filename=\"", "events.zip", "\""]
  sendResponse (TE.encodeUtf8 "application/zip", toContent $ fromArchive archive)
