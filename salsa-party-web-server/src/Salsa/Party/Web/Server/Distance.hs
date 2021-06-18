{-# LANGUAGE NumericUnderscores #-}

module Salsa.Party.Web.Server.Distance where

import Salsa.Party.Web.Server.Foundation

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
      sinSqLat = sin (latDiff / 2) ^ 2
      sinSqLon = sin (lonDiff / 2) ^ 2
      under = sinSqLat + cos lat1 * cos lat2 * sinSqLon
      -- Average radius of earth:
      r = 6_371_000 -- m
   in 2 * r * asin (sqrt under)
