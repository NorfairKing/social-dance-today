{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Geocoding where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad
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
              mCoords <-
                if debitSucceeded
                  then do
                    mOSMResult <- geocodeviaOSM query
                    case mOSMResult of
                      Just _ -> pure mOSMResult
                      -- Try using google if OSM fails.
                      Nothing -> geocodeViaGoogle googleAPIKey query
                  else pure Nothing
              case mCoords of
                Just coords -> pure $ Just coords
                Nothing -> geocodeViaGoogle googleAPIKey query

          case mCoordinates of
            Nothing -> pure Nothing
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
                        [ unwords ["Geocoding of", show query, "failed because it produced an invalid place"],
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
  logDebugNS "geocoding" $ "Geocoding using OpenStreetMaps: " <> query
  man <- asks appHTTPManager
  let req = OSM.GeocodingRequest {OSM.geocodingRequestQuery = query}
  resp <- liftIO $ OSM.makeGeocodingRequest man req
  forM (listToMaybe $ OSM.geocodingResponsePlaces resp) $ \p ->
    pure Coordinates {coordinatesLat = OSM.placeLat p, coordinatesLon = OSM.placeLon p}

geocodeViaGoogle :: (MonadReader App m, MonadLogger m, MonadIO m) => Text -> Text -> m (Maybe Coordinates)
geocodeViaGoogle key query = do
  logDebugNS "geocoding" $ "Geocoding using Google: " <> query
  man <- asks appHTTPManager
  let req = Google.GeocodingRequest {Google.geocodingRequestAddress = query, Google.geocodingRequestKey = key}
  resp <- liftIO $ Google.makeGeocodingRequest man req
  forM (listToMaybe $ Google.geocodingResponseAddresses resp) $ \a ->
    pure Coordinates {coordinatesLat = Google.addressLat a, coordinatesLon = Google.addressLon a}
