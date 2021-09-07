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
  renderUrl <- getUrlRender
  pure $ JSONResponse $ externalEventExport renderUrl externalEvent place

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

instance FromJSON PlaceExport

instance ToJSON PlaceExport

importPlaceExport :: MonadIO m => PlaceExport -> SqlPersistT m (Entity Place)
importPlaceExport PlaceExport {..} = undefined

externalEventExport :: (Route App -> Text) -> ExternalEvent -> Place -> ExternalEventExport
externalEventExport renderUrl externalEvent@ExternalEvent {..} place =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
      externalEventExportPlace = placeExport place
   in ExternalEventExport {..}

data ExternalEventExport = ExternalEventExport
  { externalEventExportPlace :: PlaceExport
  }
  deriving (Show, Eq, Generic)

instance Validity ExternalEventExport

instance ToJSON ExternalEventExport where
  toJSON ExternalEventExport {..} = object ["place" .= externalEventExportPlace]

instance FromJSON ExternalEventExport where
  parseJSON = withObject "ExternalEventExport" $ \o ->
    pure ExternalEventExport {..}

importExternalEventExport :: MonadIO m => ExternalEventExport -> SqlPersistT m (Entity ExternalEvent)
importExternalEventExport ExternalEventExport {..} = undefined
