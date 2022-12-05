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
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as VS
import Debug.Trace
import GHC.Generics (Generic)
import Salsa.Party.Poster
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 10) . describe "posterCropImage" $ do
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
    imageWidth <- elements [160, 320, 640, 1280]
    imageHeight <- elements [90, 180, 360, 640]
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
