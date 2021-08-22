{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.CoordinatesSpec (spec) where

import Salsa.Party.DB.Coordinates
import Salsa.Party.Web.Server.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist
import Text.Printf

spec :: Spec
spec = do
  genValidSpec @Latitude
  genValidSpec @Longitude
  eqSpecOnValid @Latitude
  eqSpecOnValid @Longitude
  ordSpecOnValid @Latitude
  ordSpecOnValid @Longitude
  showReadSpecOnValid @Latitude
  showReadSpecOnValid @Longitude
  persistSpecOnValid @Latitude
  persistSpecOnValid @Longitude
  jsonSpecOnValid @Latitude
  jsonSpecOnValid @Longitude
  let zurichMainStation = Coordinates {coordinatesLat = 47.3778579, coordinatesLon = 8.5381339}
  let zurichPrimeTower = Coordinates {coordinatesLat = 47.3861804, coordinatesLon = 8.5150251}
  let zurichBlatterWiese = Coordinates {coordinatesLat = 47.3547140, coordinatesLon = 8.5512022}
  let londonVictoria = Coordinates {coordinatesLat = 51.4952237, coordinatesLon = -0.1438952}
  let shouldBeCloseTo :: Double -> Double -> IO ()
      shouldBeCloseTo x y =
        let diff = abs (x - y)
            tollerance = 1000 -- Within 1km is more than close enough.
            p = printf "%9.2f"
            ctx =
              unlines
                [ "x:          " <> p x,
                  "y:          " <> p y,
                  "tollerance: " <> p tollerance
                ]
         in if diff < tollerance
              then pure ()
              else context ctx $ x `shouldBe` y
  describe "distanceTo" $ do
    it "is close enough for the zurich main station to prime tower" $ do
      zurichMainStation `distanceTo` zurichPrimeTower `shouldBeCloseTo` 1_970 -- m
    it "is close enough for the zurich main station to Blatterwiese" $ do
      zurichMainStation `distanceTo` zurichBlatterWiese `shouldBeCloseTo` 2_700 -- m
    it "is close enough for the zurich prime tower to Blatterwiese" $ do
      zurichPrimeTower `distanceTo` zurichBlatterWiese `shouldBeCloseTo` 4_340 -- m
    it "is close enough for the zurich main station to London Victoria station" $ do
      zurichMainStation `distanceTo` londonVictoria `shouldBeCloseTo` 776_290 -- m
