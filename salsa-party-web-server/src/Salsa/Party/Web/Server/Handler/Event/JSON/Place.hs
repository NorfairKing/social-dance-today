{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.JSON.Place
  ( PlaceExport (..),
    placeExport,
    importPlaceExport,
  )
where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Import

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
