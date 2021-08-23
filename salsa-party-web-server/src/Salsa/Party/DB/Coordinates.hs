{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
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
--  * A longitude is in the range [-180, 180] where -180 is the same as 180.
--
--  * The precision that we use matters.
--    See https://xkcd.com/2170/ for more details.
--    At the moment we are using Nano, but that's too precise
module Salsa.Party.DB.Coordinates
  ( Latitude,
    unLatitude,
    mkLatitude,
    Longitude,
    unLongitude,
    mkLongitude,
    Coordinates (..),
    distanceTo,
  )
where

import Control.Arrow (left)
import Data.Aeson as JSON
import Data.Fixed
import Data.Proxy
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read

newtype Latitude = Latitude {unLatitude :: Nano}
  deriving
    ( Eq,
      Ord,
      Generic
    )

instance Num Latitude where
  fromInteger i = case mkLatitudeOrError (fromInteger i) of
    Left err -> error err
    Right l -> l

  -- We have to implement negate to support literals correctly,
  -- because this expression:
  -- > (- 50 :: Latitude)
  -- is interpreted as
  -- > negate (50 :: Latitude)
  -- instead of
  -- > Latitude (-50)
  negate :: Latitude -> Latitude
  negate = Latitude . negate . unLatitude

  (+) :: Latitude -> Latitude -> Latitude
  (+) = error "It makes no sense to add latitudes."
  (-) :: Latitude -> Latitude -> Latitude
  (-) = error "It makes no sense to subtract latitudes."
  (*) :: Latitude -> Latitude -> Latitude
  (*) = error "It makes no sense to multiply latitudes."
  abs :: Latitude -> Latitude
  abs = error "It makes no sense to take the absolute value of latitudes."
  signum :: Latitude -> Latitude
  signum = error "It makes no sense to take the sign of latitudes."

instance Fractional Latitude where
  fromRational :: Rational -> Latitude
  fromRational r = case mkLatitudeOrError (fromRational r) of
    Left err -> error err
    Right l -> l

  (/) :: Latitude -> Latitude -> Latitude
  (/) = error "It makes no sense to divide latitudes."

instance Real Latitude where
  -- This function promises more precision, which isn't there, but at least not less.
  toRational :: Latitude -> Rational
  toRational = toRational . unLatitude

mkLatitude :: Nano -> Maybe Latitude
mkLatitude = constructValid . Latitude

mkLatitudeOrError :: Nano -> Either String Latitude
mkLatitudeOrError = prettyValidate . Latitude

mkLatitudeOrFail :: MonadFail m => Nano -> m Latitude
mkLatitudeOrFail n = case mkLatitudeOrError n of
  Left err -> fail err
  Right l -> pure l

instance Validity Latitude where
  validate lat@Latitude {..} =
    mconcat
      [ genericValidate lat,
        declare "Is -90 or more" $ unLatitude >= -90,
        declare "Is 90 or less" $ unLatitude <= 90
      ]

instance Show Latitude where
  show = show . unLatitude

instance Read Latitude where
  readPrec = readPrec >>= mkLatitudeOrFail

instance ToJSON Latitude where
  toJSON = toJSON . unLatitude

instance FromJSON Latitude where
  parseJSON v = parseJSON v >>= mkLatitudeOrFail

instance PersistField Latitude where
  toPersistValue = toPersistValue . unLatitude
  fromPersistValue pv = fromPersistValue pv >>= (left T.pack . mkLatitudeOrError)

instance PersistFieldSql Latitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Nano)

newtype Longitude = Longitude {unLongitude :: Nano}
  deriving
    ( Eq,
      Ord,
      Generic
    )

instance Num Longitude where
  fromInteger i = case mkLongitudeOrError (fromInteger i) of
    Left err -> error err
    Right l -> l

  -- We have to implement negate to support literals correctly,
  -- because this expression:
  -- > (- 50 :: Longitude)
  -- is interpreted as
  -- > negate (50 :: Longitude)
  -- instead of
  -- > Longitude (-50)
  negate :: Longitude -> Longitude
  negate = Longitude . negate . unLongitude

  (+) :: Longitude -> Longitude -> Longitude
  (+) = error "It makes no sense to add longitudes."
  (-) :: Longitude -> Longitude -> Longitude
  (-) = error "It makes no sense to subtract longitudes."
  (*) :: Longitude -> Longitude -> Longitude
  (*) = error "It makes no sense to multiply longitudes."
  abs :: Longitude -> Longitude
  abs = error "It makes no sense to take the absolute value of longitudes."
  signum :: Longitude -> Longitude
  signum = error "It makes no sense to take the sign of longitudes."

instance Fractional Longitude where
  fromRational :: Rational -> Longitude
  fromRational r = case mkLongitudeOrError (fromRational r) of
    Left err -> error err
    Right l -> l

  (/) :: Longitude -> Longitude -> Longitude
  (/) = error "It makes no sense to divide latitudes."

instance Real Longitude where
  -- This function promises more precision, which isn't there, but at least not less.
  toRational :: Longitude -> Rational
  toRational = toRational . unLongitude

mkLongitude :: Nano -> Maybe Longitude
mkLongitude = constructValid . Longitude

mkLongitudeOrError :: Nano -> Either String Longitude
mkLongitudeOrError = prettyValidate . Longitude

mkLongitudeOrFail :: MonadFail m => Nano -> m Longitude
mkLongitudeOrFail n = case mkLongitudeOrError n of
  Left err -> fail err
  Right l -> pure l

instance Validity Longitude where
  validate lon@Longitude {..} =
    mconcat
      [ genericValidate lon,
        declare "Is -180 or more" $ unLongitude >= -180,
        declare "Is 180 or less" $ unLongitude <= 180
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
  sqlType Proxy = sqlType (Proxy :: Proxy Nano)

data Coordinates = Coordinates
  { coordinatesLat :: !Latitude,
    coordinatesLon :: !Longitude
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Coordinates

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
