{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.Coordinates where

import Data.Fixed
import Data.Validity
import GHC.Generics (Generic)
import Yesod

data Coordinates = Coordinates
  { coordinatesLat :: !Nano,
    coordinatesLon :: !Nano
  }
  deriving (Show, Eq, Generic)

instance Validity Coordinates

-- This could potentially be dangerous if a type is read than written
instance PathPiece (Fixed a) where
  fromPathPiece = fmap MkFixed . fromPathPiece
  toPathPiece (MkFixed i) = toPathPiece i

-- See #https://en.wikipedia.org/wiki/Haversine_formula#Formulation
distanceTo ::
  Coordinates ->
  Coordinates ->
  -- | In metres
  Double
distanceTo co1 co2 =
  let toRadians = (* (pi / 180))
      lat1 = toRadians $ realToFrac (coordinatesLat co1) :: Double
      lat2 = toRadians $ realToFrac (coordinatesLat co2) :: Double
      lon1 = toRadians $ realToFrac (coordinatesLon co1) :: Double
      lon2 = toRadians $ realToFrac (coordinatesLon co2) :: Double
      latDiff = lat2 - lat1
      lonDiff = lon2 - lon1
      sinSqLat = sin (latDiff / 2) ^ (2 :: Int)
      sinSqLon = sin (lonDiff / 2) ^ (2 :: Int)
      under = sinSqLat + cos lat1 * cos lat2 * sinSqLon
      -- Average radius of earth:
      r = 6_371_000 -- m
   in 2 * r * asin (sqrt under)
