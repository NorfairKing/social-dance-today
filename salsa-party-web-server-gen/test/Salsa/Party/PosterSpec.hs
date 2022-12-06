{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Salsa.Party.PosterSpec (spec) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import GHC.Generics (Generic)
import Path
import Path.IO
import Salsa.Party.Poster
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "computeNewDimensions" $ do
    let forAllWidth :: Testable prop => (Int -> prop) -> Property
        forAllWidth = forAll (choose (1, 3 * max desiredLandscapeWidth desiredPortraitWidth))
    let forAllHeight :: Testable prop => (Int -> prop) -> Property
        forAllHeight = forAll (choose (1, 3 * max desiredLandscapeHeight desiredPortraitHeight))
    it "always makes images small enough" $
      forAllWidth $ \w ->
        forAllHeight $ \h ->
          let (w', h') = fromMaybe (w, h) $ computeNewDimensions (w, h)
           in case computeOrientation (w, h) of
                Landscape -> w' <= desiredLandscapeWidth && h' <= desiredLandscapeHeight
                _ -> w' <= desiredPortraitWidth && h' <= desiredPortraitHeight
    it "keeps the same ratio" $
      forAllWidth $ \w ->
        forAllHeight $ \h ->
          case computeNewDimensions (w, h) of
            Nothing -> pure ()
            Just t@(w', h') -> context (show t) $ 1 - ((w' % h') / (w % h)) `shouldSatisfy` (< 5 / 100) -- Less than five percent off
    it "works for this portrait image under the ratio" $
      computeNewDimensions (300, 800) `shouldBe` Just (244, 650)
    it "works for this portrait image over the ratio" $
      computeNewDimensions (700, 800) `shouldBe` Just (520, 594)
    it "works for this square image" $
      computeNewDimensions (800, 800) `shouldBe` Just (360, 360)
    it "works for this landscape image under the ratio" $
      computeNewDimensions (800, 600) `shouldBe` Just (480, 360)
    it "works for this landscape image above the ratio" $
      computeNewDimensions (800, 300) `shouldBe` Just (640, 240)

  modifyMaxSuccess (`div` 10) . describe "posterCropImage" $ do
    it "always results in a jpeg file that is small enough when starting with a jpeg file" $ do
      forAllValid $ \(Hidden jpegImage) -> do
        saveJpgImage 100 "/home/syd/segfault.jpg" (ImageYCbCr8 jpegImage)
        case posterCropImage "image/jpeg" (LB.toStrict (encodeJpeg jpegImage)) of
          Left err -> expectationFailure $ unwords ["Failed to encode jpeg", err]
          Right (typ, converted) -> do
            typ `shouldBe` "image/jpeg"
            SB.length converted `shouldSatisfy` (<= desiredSize)

    it "always results in a jpeg file that is small enough when starting with a png file" $ do
      forAllValid $ \(Hidden pngImage) ->
        case posterCropImage "image/png" (LB.toStrict (encodePng (pngImage :: Image PixelRGBA16))) of
          Left err -> expectationFailure $ unwords ["Failed to encode jpeg", err]
          Right (typ, converted) -> do
            typ `shouldBe` "image/jpeg"
            SB.length converted `shouldSatisfy` (<= desiredSize)

    it "is idempotent" $ do
      forAllValid $ \(Hidden jpegImage) -> do
        case posterCropImage "image/jpeg" (LB.toStrict (encodeJpeg (jpegImage :: Image PixelYCbCr8))) of
          Left err -> expectationFailure $ unwords ["Failed to encode jpeg", err]
          Right (typ1, convertedOnce) ->
            case posterCropImage typ1 convertedOnce of
              Left err -> expectationFailure $ unwords ["Failed to encode jpeg", err]
              Right (typ2, convertedTwice) -> do
                typ1 `shouldBe` typ2
                convertedTwice `shouldBe` convertedOnce

    scenarioDir "test_resources/posters/input" $ \inputFile ->
      it (unwords ["imports", inputFile, "the same way as before"]) $ do
        let outputFile = T.unpack . T.replace "input" "output" . T.pack $ inputFile
        contents <- SB.readFile inputFile
        case posterCropImage "image/jpeg" contents of
          Left err -> expectationFailure $ unwords ["Failed to encode jpeg", err]
          Right (_, converted) -> pure $ pureGoldenPoster outputFile $ LB.fromStrict converted

    scenarioDir "test_resources/posters/static" $ \inputFile ->
      it (unwords ["imports", inputFile, "idempotently"]) $ do
        contents <- SB.readFile inputFile
        case posterCropImage "image/jpeg" contents of
          Left err -> expectationFailure $ unwords ["Failed to encode jpeg", err]
          Right (_, converted) -> converted == contents `shouldBe` True

-- | A poster with location
data Poster = Poster
  { -- | File location for comparisons
    posterFile :: !(Path Abs File),
    -- | Decoded image
    posterImage :: !ByteString
  }

-- | Make a golden test for a given poster in lazy 'LB.ByteString' form.
pureGoldenPoster :: FilePath -> LB.ByteString -> GoldenTest Poster
pureGoldenPoster fp contents =
  GoldenTest
    { goldenTestRead = do
        relFile <- parseRelFile fp
        currentDir <- getCurrentDir
        let resolvedFile = currentDir </> relFile
        mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile resolvedFile
        forM mContents $ \cts ->
          pure $
            Poster
              { posterFile = resolvedFile,
                posterImage = cts
              },
      goldenTestProduce = do
        let sb = LB.toStrict contents
        relFile <- parseRelFile fp
        tempDir <- resolveDir' "poster-comparison"
        let tempFile = tempDir </> relFile
        ensureDir $ parent tempFile
        -- Write it to a file so we can compare it if it differs.
        SB.writeFile (fromAbsFile tempFile) sb
        pure $
          Poster
            { posterFile = tempFile,
              posterImage = sb
            },
      goldenTestWrite = \(Poster _ actual) -> do
        relFile <- parseRelFile fp
        currentDir <- getCurrentDir
        let resolvedFile = currentDir </> relFile
        ensureDir $ parent resolvedFile
        SB.writeFile (fromAbsFile resolvedFile) actual,
      goldenTestCompare = \(Poster actualPath actual) (Poster expectedPath expected) ->
        if actual == expected
          then Nothing
          else
            Just $
              ExpectationFailed $
                unlines
                  [ "Posters differ.",
                    "expected: " <> fromAbsFile expectedPath,
                    "actual: " <> fromAbsFile actualPath
                  ]
    }

newtype Hidden a = Hidden {unHidden :: a}
  deriving (Eq, Generic)

instance Show (Hidden a) where
  show _ = "hidden"

instance Validity a => Validity (Hidden a)

instance GenValid a => GenValid (Hidden a)

instance (VS.Storable (PixelBaseComponent a), Validity (PixelBaseComponent a)) => Validity (Image a) where
  validate Image {..} =
    mconcat
      [ delve "imageWidth" imageWidth,
        delve "imageHeight" imageHeight,
        delve "imageData" imageData
      ]

instance (VS.Storable (PixelBaseComponent a), Pixel a, GenValid (PixelBaseComponent a)) => GenValid (Image a) where
  shrinkValid _ = [] -- TODO
  genValid = do
    let sizes = [80, 160, 320, 640, 1280, 2560]
    imageWidth <- elements sizes
    imageHeight <- elements sizes
    imageData <-
      VS.force
        <$> VS.replicateM
          ( imageWidth
              * imageHeight
              -- I have no idea why the componentCount here is necessary
              * componentCount (undefined :: a)
          )
          genValid
    pure Image {..}

instance Validity PixelYCbCrK8 where
  validate (PixelYCbCrK8 y cb cr b) =
    mconcat
      [ delve "y" y,
        delve "cb" cb,
        delve "cr" cr,
        delve "b" b
      ]

instance GenValid PixelYCbCrK8 where
  shrinkValid _ = [] -- TODO
  genValid =
    PixelYCbCrK8
      <$> genValid
      <*> genValid
      <*> genValid
      <*> genValid

instance Validity PixelRGBA16 where
  validate (PixelRGBA16 r g b a) =
    mconcat
      [ delve "r" r,
        delve "g" g,
        delve "b" b,
        delve "a" a
      ]

instance GenValid PixelRGBA16 where
  shrinkValid _ = [] -- TODO
  genValid =
    PixelRGBA16
      <$> genValid
      <*> genValid
      <*> genValid
      <*> genValid
