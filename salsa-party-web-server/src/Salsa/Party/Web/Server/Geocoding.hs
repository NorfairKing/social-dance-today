{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Geocoding where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import qualified Google.Geocoding as Google
import qualified OpenStreetMaps.Geocoding as OSM
import Salsa.Party.AdminNotification
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import Yesod

lookupPlace :: Text -> Handler (Entity Place)
lookupPlace query = do
  app <- getYesod
  mr <- runReaderT (lookupPlaceRaw query) app
  case mr of
    Nothing -> invalidArgs ["Place not found: " <> query]
    Just p -> pure p

lookupPlaceRaw :: (MonadUnliftIO m, MonadReader App m, MonadLoggerIO m) => Text -> m (Maybe (Entity Place))
lookupPlaceRaw query = do
  if T.null query
    then do
      logWarnNS "geocoding" $
        T.pack $
          unwords
            [ "Not geocoding because the query is empty:",
              show query
            ]
      pure Nothing
    else do
      mPlace <- appDB $ getBy $ UniquePlaceQuery query
      case mPlace of
        Just pe -> do
          logDebugNS "geocoding" $
            T.pack $
              unwords
                [ "Found place in cache, not gecoding query:",
                  show query
                ]
          pure $ Just pe
        Nothing -> do
          logDebugNS "geocoding" $
            T.pack $
              unwords
                [ "Did not find place in cache, gecoding query:",
                  show query
                ]

          mOSMRateLimiter <- asks appOSMRateLimiter
          mGoogleAPIKey <- asks appGoogleAPIKey

          mCoordinates <- case (mOSMRateLimiter, mGoogleAPIKey) of
            (Nothing, Nothing) -> do
              let msg =
                    T.concat
                      [ "No geocoding service configured to geocode ",
                        T.pack (show query),
                        ", please contact the site administrators."
                      ]
              logErrorNS "geocoding" msg
              sendAdminNotification msg
              pure Nothing
            (Just osmRateLimiter, Nothing) -> do
              liftIO $ waitDebit osmRateLimiter 1
              geocodeviaOSM query
            (Nothing, Just googleAPIKey) -> do
              geocodeViaGoogle googleAPIKey query
            (Just osmRateLimiter, Just googleAPIKey) -> do
              debitSucceeded <- liftIO $ tryDebit osmRateLimiter 1
              if debitSucceeded
                then do
                  mOSMResult <- geocodeviaOSM query
                  case mOSMResult of
                    Just _ -> pure mOSMResult
                    -- Try using google if OSM fails.
                    Nothing -> geocodeViaGoogle googleAPIKey query
                else -- Try using google if we can't use OSM due to rate-limiting
                  geocodeViaGoogle googleAPIKey query

          case mCoordinates of
            Nothing -> do
              logWarnNS "geocoding" $ T.pack $ unwords ["Failed to geocode", show query]
              pure Nothing
            Just coordinates@Coordinates {..} -> do
              logInfoNS "geocoding" $ T.pack $ unwords ["Geocoded", show query, "to", show coordinates]
              let place =
                    Place
                      { placeQuery = query,
                        placeLat = coordinatesLat,
                        placeLon = coordinatesLon
                      }
              case prettyValidate place of
                Left err -> do
                  logErrorNS "geocoding" $
                    T.pack $
                      unlines
                        [ unwords
                            [ "Geocoding of",
                              show query,
                              "failed because it produced an invalid place"
                            ],
                          err
                        ]
                  pure Nothing
                Right _ -> do
                  fmap Just $
                    appDB $
                      upsertBy
                        (UniquePlaceQuery query)
                        place
                        [ PlaceLat =. coordinatesLat,
                          PlaceLon =. coordinatesLon
                        ]

geocodeviaOSM :: (MonadReader App m, MonadLogger m, MonadIO m) => Text -> m (Maybe Coordinates)
geocodeviaOSM query = do
  logDebugNS "geocoding-google" $ "Geocoding using OpenStreetMaps: " <> query
  man <- asks appHTTPManager
  let req = OSM.GeocodingRequest {OSM.geocodingRequestQuery = query}
  resp <- liftIO $ OSM.makeGeocodingRequest man req
  case listToMaybe $ OSM.geocodingResponsePlaces resp of
    Just p -> pure $ Just Coordinates {coordinatesLat = OSM.placeLat p, coordinatesLon = OSM.placeLon p}
    Nothing -> do
      logWarnNS "geocoding-openstreetmaps" $ "Failed to geocode using OpenStreetMaps: " <> query
      pure Nothing

geocodeViaGoogle :: (MonadReader App m, MonadLogger m, MonadIO m) => Text -> Text -> m (Maybe Coordinates)
geocodeViaGoogle key query = do
  logDebugNS "geocoding-google" $ "Geocoding using Google: " <> query
  man <- asks appHTTPManager
  let req = Google.GeocodingRequest {Google.geocodingRequestAddress = query, Google.geocodingRequestKey = key}
  resp <- liftIO $ Google.makeGeocodingRequest man req
  case listToMaybe $ Google.geocodingResponseAddresses resp of
    Just a -> pure $ Just Coordinates {coordinatesLat = Google.addressLat a, coordinatesLon = Google.addressLon a}
    Nothing -> do
      logWarnNS "geocoding-google" $ "Failed to geocode using Google: " <> query
      pure Nothing
