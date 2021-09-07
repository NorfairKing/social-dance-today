{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.CoordinatesSpec (spec) where

import Data.Fixed
import Salsa.Party.DB.Coordinates
import Salsa.Party.Web.Server.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist
import Text.Printf
import Web.PathPieces

spec :: Spec
spec = do
  describe "Coord" $ do
    genValidSpec @Coord
    eqSpecOnValid @Coord
    ordSpecOnValid @Coord
    showReadSpecOnValid @Coord
    persistSpecOnValid @Coord
    jsonSpecOnValid @Coord

  describe "Latitude" $ do
    genValidSpec @Latitude
    eqSpecOnValid @Latitude
    ordSpecOnValid @Latitude
    showReadSpecOnValid @Latitude
    persistSpecOnValid @Latitude
    jsonSpecOnValid @Latitude

    describe "mkLatitude" $ do
      it "works with this example" $ do
        mkLatitude (- 110) `shouldBe` Nothing
      it "works with this example" $ do
        mkLatitude (- 60) `shouldBe` Just (Latitude (- 60))
      it "works with this example" $ do
        mkLatitude (-15) `shouldBe` Just (Latitude (Coord (-15_00000)))
      it "works with the lower boundary" $ do
        mkLatitude (- 90) `shouldBe` Just (Latitude (- 90))
      it "works with the upper boundary" $ do
        mkLatitude 90 `shouldBe` Just (Latitude 90)
      it "works with this example" $ do
        mkLatitude 70 `shouldBe` Just (Latitude 70)
      it "works with this example" $ do
        mkLatitude 100 `shouldBe` Nothing
      it "works with this example" $ do
        mkLatitude 300 `shouldBe` Nothing

      it "produces valid latitudes" $ producesValidsOnValids mkLatitude

  describe "Longitude" $ do
    genValidSpec @Longitude
    eqSpecOnValid @Longitude
    ordSpecOnValid @Longitude
    showReadSpecOnValid @Longitude
    persistSpecOnValid @Longitude
    jsonSpecOnValid @Longitude

    describe "mkLongitude" $ do
      it "works with this example" $ do
        mkLongitude (- 410) `shouldBe` Nothing
      it "works with this example" $ do
        mkLongitude (-210) `shouldBe` Nothing
      it "works with this example" $ do
        mkLongitude (- 40) `shouldBe` Just (Longitude (- 40))
      it "works with this example" $ do
        mkLongitude 15 `shouldBe` Just (Longitude (Coord 15_00000))
      it "works with the upper boundary" $ do
        mkLongitude 180 `shouldBe` Nothing
      it "works with the lower boundary" $ do
        mkLongitude (- 180) `shouldBe` Just (Longitude (- 180))
      it "works with the upper boundary" $ do
        mkLongitude (180 - fixedToCoord (MkFixed 1)) `shouldBe` Just (Longitude (180 - fixedToCoord (MkFixed 1)))
      it "works with this example" $ do
        mkLongitude 50 `shouldBe` Just (Longitude 50)
      it "works with this example" $ do
        mkLongitude 200 `shouldBe` Nothing
      it "works with this example" $ do
        mkLongitude 410 `shouldBe` Nothing

      it "produces valid longitudes" $ producesValidsOnValids mkLongitude

  describe "Coordinates" $ do
    describe "PathPiece" $ do
      it "roundtrips" $ do
        forAllValid $ \coordinates ->
          fromPathPiece (toPathPiece coordinates) `shouldBe` Just (coordinates :: Coordinates)

      it "outputs zurich main station's coordinates correctly" $
        toPathPiece
          (Coordinates {coordinatesLat = Latitude 47.3778579, coordinatesLon = Longitude 8.5381339})
          `shouldBe` "47.37785,8.53813"

    describe "distanceTo" $ do
      let zurichMainStation = Coordinates {coordinatesLat = Latitude 47.3778579, coordinatesLon = Longitude 8.5381339}
      let zurichPrimeTower = Coordinates {coordinatesLat = Latitude 47.3861804, coordinatesLon = Longitude 8.5150251}
      let zurichBlatterWiese = Coordinates {coordinatesLat = Latitude 47.3547140, coordinatesLon = Longitude 8.5512022}
      let londonVictoria = Coordinates {coordinatesLat = Latitude 51.4952237, coordinatesLon = Longitude (-0.1438952)}
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
      it "is close enough for the zurich main station to prime tower" $ do
        zurichMainStation `distanceTo` zurichPrimeTower `shouldBeCloseTo` 1_970 -- m
      it "is close enough for the zurich main station to Blatterwiese" $ do
        zurichMainStation `distanceTo` zurichBlatterWiese `shouldBeCloseTo` 2_700 -- m
      it "is close enough for the zurich prime tower to Blatterwiese" $ do
        zurichPrimeTower `distanceTo` zurichBlatterWiese `shouldBeCloseTo` 4_340 -- m
      it "is close enough for the zurich main station to London Victoria station" $ do
        zurichMainStation `distanceTo` londonVictoria `shouldBeCloseTo` 776_290 -- m
