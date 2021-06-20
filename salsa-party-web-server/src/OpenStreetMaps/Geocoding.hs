{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenStreetMaps.Geocoding where

import Control.Concurrent.TokenLimiter
import Control.Exception
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Fixed
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Text.Read

-- From https://nominatim.org/release-docs/develop/api/Search/#search-queries
--
-- Terms:
--
-- No heavy uses (an absolute maximum of 1 request per second).
data GeocodingRequest = GeocodingRequest
  { geocodingRequestQuery :: Text
  }
  deriving (Show, Eq, Generic)

newtype GeocodingResponse = GeocodingResponse {geocodingResponsePlaces :: [Place]}
  deriving (Show, Eq, Generic)

instance FromJSON GeocodingResponse where
  parseJSON = fmap GeocodingResponse . parseJSON

data Place = Place
  { placeLat :: !Nano,
    placeLon :: !Nano,
    placeImportance :: !Double,
    placeRank :: !Double,
    placeDisplayName :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Place where
  parseJSON = withObject "Place" $ \o ->
    Place
      <$> (o .: "lat" >>= readParser)
      <*> (o .: "lon" >>= readParser)
      <*> o .: "importance"
      <*> o .: "place_rank"
      <*> o .: "display_name"

readParser :: Read a => String -> JSON.Parser a
readParser s = case readMaybe s of
  Nothing -> fail $ "Failed to parse: " <> s
  Just a -> pure a

data GeocodingException = DecodingGeocodingResponseFailed String
  deriving (Show)

instance Exception GeocodingException

-- https://operations.osmfoundation.org/policies/nominatim
-- says: "an absolute maximum of 1 request per second"

limitConfig :: LimitConfig
limitConfig =
  defaultLimitConfig
    { maxBucketTokens = 1,
      initialBucketTokens = 1,
      bucketRefillTokensPerSecond = 1
    }

makeGeocodingRequest :: HTTP.Manager -> GeocodingRequest -> IO GeocodingResponse
makeGeocodingRequest manager GeocodingRequest {..} = do
  requestPrototype <- parseRequest "https://nominatim.openstreetmap.org/search"
  let request =
        setQueryString
          [("q", Just $ TE.encodeUtf8 geocodingRequestQuery), ("format", Just "jsonv2")]
          requestPrototype
            { requestHeaders = [("User-Agent", "salsa-parties.today")]
            }
  response <- httpLbs request manager
  case eitherDecode' (responseBody response) of
    -- We throw this exception because it should not happen and we can't fix it.
    Left err -> throwIO $ DecodingGeocodingResponseFailed err
    Right gcr -> pure $ GeocodingResponse $ sortBy (comparing placeRank <> comparing (Down . placeImportance)) gcr
