{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.Coordinates where

import Data.Fixed
import Data.Proxy
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Yesod

newtype Latitude = Latitude {unLatitude :: Nano}
  deriving
    ( Eq,
      Ord,
      Generic,
      Num,
      Fractional,
      Real,
      FromJSON,
      ToJSON
    )

instance Validity Latitude

instance Show Latitude where
  show = show . unLatitude

instance Read Latitude where
  readPrec = Latitude <$> readPrec

instance PersistField Latitude where
  toPersistValue = toPersistValue . unLatitude
  fromPersistValue = fmap Latitude . fromPersistValue

instance PersistFieldSql Latitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Nano)

newtype Longitude = Longitude {unLongitude :: Nano}
  deriving
    ( Eq,
      Ord,
      Generic,
      Num,
      Fractional,
      Real,
      FromJSON,
      ToJSON
    )

instance Validity Longitude

instance Show Longitude where
  show = show . unLongitude

instance Read Longitude where
  readPrec = Longitude <$> readPrec

instance PersistField Longitude where
  toPersistValue = toPersistValue . unLongitude
  fromPersistValue = fmap Longitude . fromPersistValue

instance PersistFieldSql Longitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Nano)

data Coordinates = Coordinates
  { coordinatesLat :: !Latitude,
    coordinatesLon :: !Longitude
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Coordinates

-- See #https://en.wikipedia.org/wiki/Haversine_formula#Formulation
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
