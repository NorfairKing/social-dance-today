{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Google.Geocoding where

import Control.Exception
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Retry as HTTP
import Salsa.Party.DB.Coordinates

-- From https://nominatim.org/release-docs/develop/api/Search/#search-queries
--
-- Terms:
--
-- No heavy uses (an absolute maximum of 1 request per second).
data GeocodingRequest = GeocodingRequest
  { geocodingRequestAddress :: !Text,
    geocodingRequestKey :: !Text
  }
  deriving (Show, Eq, Generic)

newtype GeocodingResponse = GeocodingResponse {geocodingResponseAddresses :: [Address]}
  deriving (Show, Eq, Generic)

instance FromJSON GeocodingResponse where
  parseJSON = withObject "GeocodingResponse" $ \o -> GeocodingResponse <$> o .:? "results" .!= []

data Address = Address
  { addressLat :: !Latitude,
    addressLon :: !Longitude
  }
  deriving (Show, Eq, Generic)

instance FromJSON Address where
  parseJSON = withObject "Address" $ \o -> do
    geometry <- o .: "geometry"
    location <- geometry .: "location"
    Address <$> location .: "lat" <*> location .: "lng"

-- TODO deal with status codes other than ok
data GeocodingException = DecodingGeocodingResponseFailed String
  deriving (Show)

instance Exception GeocodingException

makeGeocodingRequest :: HTTP.Manager -> GeocodingRequest -> IO GeocodingResponse
makeGeocodingRequest manager GeocodingRequest {..} = do
  requestPrototype <- parseRequest "https://maps.googleapis.com/maps/api/geocode/json"
  let request =
        setQueryString
          [("address", Just $ TE.encodeUtf8 geocodingRequestAddress), ("key", Just $ TE.encodeUtf8 geocodingRequestKey), ("language", Just "en-GB")]
          requestPrototype
            { requestHeaders = [("User-Agent", "social-dance.today")]
            }
  errOrResponse <- runNoLoggingT $ httpLbsWithRetry request manager
  case errOrResponse of
    Left httpException -> throwIO httpException
    Right response ->
      case eitherDecode' (responseBody response) of
        -- We throw this exception because it should not happen and we can't fix it.
        Left err -> throwIO $ DecodingGeocodingResponseFailed err
        Right gcr -> pure gcr
