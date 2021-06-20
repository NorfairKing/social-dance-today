{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Geocoding where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Google.Geocoding as Google
import qualified OpenStreetMaps.Geocoding as OSM
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Foundation
import Yesod

lookupPlace :: Text -> Handler (Entity Place)
lookupPlace query = do
  mPlace <- runDB $ getBy $ UniquePlaceQuery query
  case mPlace of
    Just pe -> do
      logDebugNS "geocoding" $ "Found place in cache, not gecoding query: " <> query
      pure pe
    Nothing -> do
      logDebugNS "geocoding" $ "Did not find place in cache, gecoding query: " <> query
      man <- getsYesod appHTTPManager
      let req = OSM.GeocodingRequest {OSM.geocodingRequestQuery = query}
      resp <- liftIO $ OSM.makeGeocodingRequest man req
      case listToMaybe $ OSM.geocodingResponsePlaces resp of
        Nothing -> invalidArgs ["Place not found."]
        Just p ->
          runDB $
            upsertBy
              (UniquePlaceQuery query)
              ( Place
                  { placeQuery = query,
                    placeLat = OSM.placeLat p,
                    placeLon = OSM.placeLon p
                  }
              )
              [PlaceLat =. OSM.placeLat p, PlaceLon =. OSM.placeLon p]

geocodeViaGoogle :: Text -> Handler Coordinates
geocodeViaGoogle query = do
  man <- getsYesod appHTTPManager
  let req = Google.GeocodingRequest {Google.geocodingRequestQuery = query}
  resp <- liftIO $ Google.makeGeocodingRequest man req
  case listToMaybe $ google . geocodingResponseAddresses resp of
    Nothing -> invalidArgs ["Place not found."]
    Just a -> pure a
