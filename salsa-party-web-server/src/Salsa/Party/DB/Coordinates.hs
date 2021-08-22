{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
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
