{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Geocoding where

import Control.Concurrent.TokenLimiter
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Google.Geocoding as Google
import qualified OpenStreetMaps.Geocoding as OSM
import Salsa.Party.DB
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

      mOSMRateLimiter <- getsYesod appOSMRateLimiter
      mGoogleAPIKey <- getsYesod appGoogleAPIKey

      mCoordinates <- case (mOSMRateLimiter, mGoogleAPIKey) of
        (Nothing, Nothing) -> invalidArgs ["No geocoding service configured, please contact the site administrators."]
        (Just osmRateLimiter, Nothing) -> do
          liftIO $ waitDebit OSM.limitConfig osmRateLimiter 1
          geocodeviaOSM query
        (Nothing, Just googleAPIKey) -> do
          geocodeViaGoogle googleAPIKey query
        (Just osmRateLimiter, Just googleAPIKey) -> do
          debitSucceeded <- liftIO $ tryDebit OSM.limitConfig osmRateLimiter 1
          mCoords <-
            if debitSucceeded
              then geocodeviaOSM query
              else pure Nothing
          case mCoords of
            Just coords -> pure $ Just coords
            Nothing -> geocodeViaGoogle googleAPIKey query

      case mCoordinates of
        Nothing -> invalidArgs ["Place not found."]
        Just coordinates@Coordinates {..} -> do
          logInfoNS "geocoding" $ "Geocoded " <> query <> " to " <> T.pack (show coordinates)
          runDB $
            upsertBy
              (UniquePlaceQuery query)
              ( Place
                  { placeQuery = query,
                    placeLat = coordinatesLat,
                    placeLon = coordinatesLon
                  }
              )
              [ PlaceLat =. coordinatesLat,
                PlaceLon =. coordinatesLon
              ]

geocodeviaOSM :: Text -> Handler (Maybe Coordinates)
geocodeviaOSM query = do
  logDebugNS "geocoding" $ "Geocoding using OpenStreetMaps: " <> query
  man <- getsYesod appHTTPManager
  let req = OSM.GeocodingRequest {OSM.geocodingRequestQuery = query}
  resp <- liftIO $ OSM.makeGeocodingRequest man req
  forM (listToMaybe $ OSM.geocodingResponsePlaces resp) $ \p ->
    pure Coordinates {coordinatesLat = OSM.placeLat p, coordinatesLon = OSM.placeLon p}

geocodeViaGoogle :: Text -> Text -> Handler (Maybe Coordinates)
geocodeViaGoogle key query = do
  logDebugNS "geocoding" $ "Geocoding using Google: " <> query
  man <- getsYesod appHTTPManager
  let req = Google.GeocodingRequest {Google.geocodingRequestAddress = query, Google.geocodingRequestKey = key}
  resp <- liftIO $ Google.makeGeocodingRequest man req
  forM (listToMaybe $ Google.geocodingResponseAddresses resp) $ \a ->
    pure Coordinates {coordinatesLat = Google.addressLat a, coordinatesLon = Google.addressLon a}
