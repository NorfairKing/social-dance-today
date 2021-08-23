{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | We store latitude and longitude as a fixed-point number so we don't have to deal with NaNs.
--
-- This is a much deeper rabit hole than I expected:
--
--  * A latitude and longitude have a qualitative difference.
--    A longitude can be considered to wrap around modularly but a latitude rather not.
--
--  * A latitude is in the range [-90, 90] where -90 is opposite to 90.
--
--  * A longitude is in the range [-180, 180[ where -180 is the same as 180, so we choose an open interval.
--
--  * The precision that we use matters.
--    We are using five digits of precision, which corresponds to about 1m.
--    This is more than enough to point at a party.
--    See https://xkcd.com/2170/ for more details.
--
--  We made these decisions:
--
--  * Num instance for literals, but for nothing else
--  * Real and Fractional instances to be able to convert to Double for distance calculations
--  * No Enum instances because we don't need them.
module Salsa.Party.DB.Coordinates
  ( Coord,
    Latitude (..),
    mkLatitude,
    mkLatitudeOrError,
    latitudeToFloat,
    Longitude (..),
    mkLongitude,
    mkLongitudeOrError,
    longitudeToFloat,
    Coordinates (..),
    distanceTo,
  )
where

import Control.Arrow (left)
import Data.Aeson as JSON
import Data.Fixed
import Data.List
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Web.PathPieces

data E5

instance HasResolution E5 where
  resolution _ = 100_000

type Coord = Fixed E5

newtype Latitude = Latitude {unLatitude :: Coord}
  deriving
    ( Eq,
      Ord,
      Generic
    )

mkLatitude :: Coord -> Maybe Latitude
mkLatitude = constructValid . Latitude

mkLatitudeOrError :: Coord -> Either String Latitude
mkLatitudeOrError = prettyValidate . Latitude

mkLatitudeOrFail :: MonadFail m => Coord -> m Latitude
mkLatitudeOrFail n = case mkLatitudeOrError n of
  Left err -> fail err
  Right l -> pure l

instance Validity Latitude where
  validate lat@Latitude {..} =
    mconcat
      [ genericValidate lat,
        declare ("Is -90 or more: " <> show unLatitude) $ lat >= minBound,
        declare ("Is 90 or less: " <> show unLatitude) $ lat <= maxBound
      ]

instance Show Latitude where
  show = show . unLatitude

instance Read Latitude where
  readPrec = readPrec >>= mkLatitudeOrFail

instance Bounded Latitude where
  minBound = Latitude (-90)
  maxBound = Latitude 90

instance ToJSON Latitude where
  toJSON = toJSON . unLatitude

instance FromJSON Latitude where
  parseJSON v = parseJSON v >>= mkLatitudeOrFail

instance PersistField Latitude where
  toPersistValue = toPersistValue . unLatitude
  fromPersistValue pv = fromPersistValue pv >>= (left T.pack . mkLatitudeOrError)

instance PersistFieldSql Latitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Coord)

latitudeToFloat :: RealFloat f => Latitude -> f
latitudeToFloat = realToFrac . unLatitude

newtype Longitude = Longitude {unLongitude :: Coord}
  deriving
    ( Eq,
      Ord,
      Generic
    )

mkLongitude :: Coord -> Maybe Longitude
mkLongitude = constructValid . Longitude

mkLongitudeOrError :: Coord -> Either String Longitude
mkLongitudeOrError = prettyValidate . Longitude

mkLongitudeOrFail :: MonadFail m => Coord -> m Longitude
mkLongitudeOrFail n = case mkLongitudeOrError n of
  Left err -> fail err
  Right l -> pure l

instance Validity Longitude where
  validate lon@Longitude {..} =
    mconcat
      [ genericValidate lon,
        declare ("Is -180 or more: " <> show unLongitude) $ lon >= minBound,
        declare ("Is 180 or less: " <> show unLongitude) $ lon <= maxBound
      ]

instance Show Longitude where
  show = show . unLongitude

instance Read Longitude where
  readPrec = readPrec >>= mkLongitudeOrFail

instance ToJSON Longitude where
  toJSON = toJSON . unLongitude

instance FromJSON Longitude where
  parseJSON v = parseJSON v >>= mkLongitudeOrFail

instance PersistField Longitude where
  toPersistValue = toPersistValue . unLongitude
  fromPersistValue pv = fromPersistValue pv >>= (left T.pack . mkLongitudeOrError)

instance PersistFieldSql Longitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Coord)

instance Bounded Longitude where
  minBound = Longitude (-180)
  maxBound = Longitude (180 - MkFixed 1)

longitudeToFloat :: RealFloat f => Longitude -> f
longitudeToFloat = realToFrac . unLongitude

data Coordinates = Coordinates
  { coordinatesLat :: !Latitude,
    coordinatesLon :: !Longitude
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Coordinates

instance PathPiece Coordinates where
  toPathPiece :: Coordinates -> Text
  toPathPiece Coordinates {..} =
    T.pack $
      intercalate
        ","
        [ show coordinatesLat,
          show coordinatesLon
        ]

  fromPathPiece :: Text -> Maybe Coordinates
  fromPathPiece t = case T.split (== ',') t of
    [latText, lonText] -> Coordinates <$> readMaybe (T.unpack latText) <*> readMaybe (T.unpack lonText)
    _ -> Nothing

-- See #https://en.wikipedia.org/wiki/Haversine_formula#Formulation
--
-- The resulting double will be much more precise than the actual distance, so watch out.
distanceTo ::
  Coordinates ->
  Coordinates ->
  -- | In metres
  Double
distanceTo co1 co2 =
  let toRadians = (* (pi / 180))
      lat1 = toRadians $ realToFrac (unLatitude $ coordinatesLat co1) :: Double
      lat2 = toRadians $ realToFrac (unLatitude $ coordinatesLat co2) :: Double
      lon1 = toRadians $ realToFrac (unLongitude $ coordinatesLon co1) :: Double
      lon2 = toRadians $ realToFrac (unLongitude $ coordinatesLon co2) :: Double
      latDiff = lat2 - lat1
      lonDiff = lon2 - lon1
      sinSqLat = sin (latDiff / 2) ^ (2 :: Int)
      sinSqLon = sin (lonDiff / 2) ^ (2 :: Int)
      under = sinSqLat + cos lat1 * cos lat2 * sinSqLon
      -- Average radius of earth:
      r = 6_371_000 -- m
   in 2 * r * asin (sqrt under)
