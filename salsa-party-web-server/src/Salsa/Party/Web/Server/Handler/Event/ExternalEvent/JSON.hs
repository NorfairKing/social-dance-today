{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
  ( externalEventPageJSON,
    ExternalEventExport (..),
    externalEventExport,
    importExternalEventExport,
  )
where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.JSON.Place
import Salsa.Party.Web.Server.Handler.Import
import Web.JSONLD (mField)
import Yesod.Core.Types

externalEventPageJSON :: Entity ExternalEvent -> Handler (JSONResponse ExternalEventExport)
externalEventPageJSON (Entity _ externalEvent) = do
  place@Place {..} <- runDB $ get404 $ externalEventPlace externalEvent
  importerMetadata <- runDB $ get404 $ externalEventImporter externalEvent
  pure $ JSONResponse $ externalEventExport externalEvent place importerMetadata

externalEventExport :: ExternalEvent -> Place -> ImporterMetadata -> ExternalEventExport
externalEventExport ExternalEvent {..} place ImporterMetadata {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      ImporterMetadata _ _ _ _ = undefined
      externalEventExportUuid = externalEventUuid
      externalEventExportKey = externalEventKey
      externalEventExportTitle = externalEventTitle
      externalEventExportDescription = externalEventDescription
      externalEventExportOrganiser = externalEventOrganiser
      externalEventExportDay = externalEventDay
      externalEventExportStart = externalEventStart
      externalEventExportHomepage = externalEventHomepage
      externalEventExportPrice = externalEventPrice
      externalEventExportCancelled = externalEventCancelled
      externalEventExportCreated = externalEventCreated
      externalEventExportModified = externalEventModified
      externalEventExportPlace = placeExport place
      externalEventExportImporter = importerMetadataName
      externalEventExportOrigin = externalEventOrigin
   in ExternalEventExport {..}

data ExternalEventExport = ExternalEventExport
  { externalEventExportUuid :: EventUUID,
    externalEventExportKey :: Text,
    externalEventExportTitle :: Text,
    externalEventExportDescription :: !(Maybe Text),
    externalEventExportOrganiser :: !(Maybe Text),
    externalEventExportDay :: Day,
    externalEventExportStart :: !(Maybe TimeOfDay),
    externalEventExportHomepage :: !(Maybe Text),
    externalEventExportPrice :: !(Maybe Text),
    externalEventExportCancelled :: !Bool,
    externalEventExportCreated :: !UTCTime,
    externalEventExportModified :: !(Maybe UTCTime),
    externalEventExportPlace :: !PlaceExport,
    externalEventExportImporter :: !Text,
    externalEventExportOrigin :: !Text
  }
  deriving (Show, Eq, Generic)

instance Validity ExternalEventExport

instance ToJSON ExternalEventExport where
  toJSON ExternalEventExport {..} =
    let ExternalEventExport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
     in object $
          concat
            [ [ "uuid" .= externalEventExportUuid,
                "key" .= externalEventExportKey,
                "title" .= externalEventExportTitle,
                "day" .= externalEventExportDay,
                "cancelled" .= externalEventExportCancelled,
                "created" .= externalEventExportCreated,
                "place" .= externalEventExportPlace,
                "importer" .= externalEventExportImporter,
                "origin" .= externalEventExportOrigin
              ],
              mField "organiser" externalEventExportOrganiser,
              mField "description" externalEventExportDescription,
              mField "homepage" externalEventExportHomepage,
              mField "start" externalEventExportStart,
              mField "price" externalEventExportPrice,
              mField "modified" externalEventExportModified
            ]

instance FromJSON ExternalEventExport where
  parseJSON = withObject "ExternalEventExport" $ \o -> do
    externalEventExportUuid <- o .: "uuid"
    externalEventExportKey <- o .: "key"
    externalEventExportTitle <- o .: "title"
    externalEventExportDescription <- o .:? "description"
    externalEventExportOrganiser <- o .:? "organiser"
    externalEventExportDay <- o .: "day"
    externalEventExportStart <- o .:? "start"
    externalEventExportHomepage <- o .:? "homepage"
    externalEventExportPrice <- o .:? "price"
    externalEventExportCancelled <- o .:? "cancelled" .!= False
    externalEventExportCreated <- o .: "created"
    externalEventExportModified <- o .:? "modified"
    externalEventExportPlace <- o .: "place"
    externalEventExportImporter <- o .: "importer"
    externalEventExportOrigin <- o .: "origin"
    pure ExternalEventExport {..}

importExternalEventExport :: MonadIO m => ExternalEventExport -> SqlPersistT m (Entity ExternalEvent)
importExternalEventExport ExternalEventExport {..} = do
  let externalEventUuid = externalEventExportUuid
  let externalEventKey = externalEventExportKey
  let externalEventTitle = externalEventExportTitle
  let externalEventDescription = externalEventExportDescription
  let externalEventDay = externalEventExportDay
  let externalEventStart = externalEventExportStart
  let externalEventHomepage = externalEventExportHomepage
  let externalEventOrganiser = externalEventExportOrganiser
  let externalEventPrice = externalEventExportPrice
  let externalEventCancelled = externalEventExportCancelled
  let externalEventCreated = externalEventExportCreated
  let externalEventModified = externalEventExportModified
  Entity externalEventPlace _ <- importPlaceExport externalEventExportPlace
  let externalEventOrigin = externalEventExportOrigin
  Entity externalEventImporter _ <-
    upsertBy
      (UniqueImporterMetadataName externalEventExportImporter)
      ( ImporterMetadata
          { importerMetadataName = externalEventExportImporter,
            importerMetadataLastRunStart = Nothing,
            importerMetadataLastRunEnd = Nothing,
            importerMetadataLastRunImported = Nothing
          }
      )
      []
  let externalEvent = ExternalEvent {..}
  upsertBy (UniqueExternalEventKey externalEventImporter externalEventExportKey) externalEvent []
