{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
  ( externalEventPageJSON,
    PlaceExport (..),
    placeExport,
    importPlaceExport,
    ExternalEventExport (..),
    externalEventExport,
    importExternalEventExport,
  )
where

import Data.Aeson as JSON
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.URI
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar as ICal
import Yesod
import Yesod.Core.Types

externalEventPageJSON :: Entity ExternalEvent -> Handler (JSONResponse ExternalEventExport)
externalEventPageJSON (Entity _ externalEvent) = do
  place@Place {..} <- runDB $ get404 $ externalEventPlace externalEvent
  importerMetadata <- runDB $ get404 $ externalEventImporter externalEvent
  renderUrl <- getUrlRender
  pure $ JSONResponse $ externalEventExport renderUrl externalEvent place importerMetadata

placeExport :: Place -> PlaceExport
placeExport Place {..} =
  let Place _ _ _ = undefined
      placeExportAddress = placeQuery
      placeExportLatitude = placeLat
      placeExportLongitude = placeLon
   in PlaceExport {..}

data PlaceExport = PlaceExport
  { placeExportAddress :: !Text,
    placeExportLatitude :: !Latitude,
    placeExportLongitude :: !Longitude
  }
  deriving (Show, Eq, Generic)

instance Validity PlaceExport

instance FromJSON PlaceExport where
  parseJSON = withObject "PlaceExport" $ \o ->
    PlaceExport
      <$> o .: "address"
      <*> o .: "latitude"
      <*> o .: "longitude"

instance ToJSON PlaceExport where
  toJSON PlaceExport {..} =
    object
      [ "address" .= placeExportAddress,
        "latitude" .= placeExportLatitude,
        "longitude" .= placeExportLongitude
      ]

importPlaceExport :: MonadIO m => PlaceExport -> SqlPersistT m (Entity Place)
importPlaceExport PlaceExport {..} =
  upsertBy
    (UniquePlaceQuery placeExportAddress)
    ( Place
        { placeQuery = placeExportAddress,
          placeLat = placeExportLatitude,
          placeLon = placeExportLongitude
        }
    )
    [ PlaceLat =. placeExportLatitude,
      PlaceLon =. placeExportLongitude
    ]

externalEventExport :: (Route App -> Text) -> ExternalEvent -> Place -> ImporterMetadata -> ExternalEventExport
externalEventExport renderUrl ExternalEvent {..} place ImporterMetadata {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
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
     in object
          [ "uuid" .= externalEventExportUuid,
            "key" .= externalEventExportKey,
            "title" .= externalEventExportTitle,
            "description" .= externalEventExportDescription,
            "organiser" .= externalEventExportOrganiser,
            "day" .= externalEventExportDay,
            "start" .= externalEventExportStart,
            "homepage" .= externalEventExportHomepage,
            "price" .= externalEventExportPrice,
            "cancelled" .= externalEventExportCancelled,
            "created" .= externalEventExportCreated,
            "modified" .= externalEventExportModified,
            "place" .= externalEventExportPlace,
            "importer" .= externalEventExportImporter,
            "origin" .= externalEventExportOrigin
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
