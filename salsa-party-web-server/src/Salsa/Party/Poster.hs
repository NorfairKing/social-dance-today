{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Salsa.Party.Poster
  ( desiredLandscapeWidth,
    desiredLandscapeHeight,
    desiredPortraitWidth,
    desiredPortraitHeight,
    desiredSize,
    computeOrientation,
    Orientation (..),
    computeNewDimensions,
    posterCropImage,
  )
where

import Codec.Picture
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Jpg
import Codec.Picture.Metadata
import Codec.Picture.Types
import Data.Bits (unsafeShiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Ratio
import Data.Text (Text)
import qualified Data.Vector.Storable as VS
import Data.Word

-- Landscape dimensions: 16:9
-- In pixels
desiredLandscapeWidth :: Int
desiredLandscapeWidth = 640

-- In pixels
desiredLandscapeHeight :: Int
desiredLandscapeHeight = 360

-- Portrait dimensions: 5:4
-- In pixels
desiredPortraitWidth :: Int
desiredPortraitWidth = 520

-- In pixels
desiredPortraitHeight :: Int
desiredPortraitHeight = 650

-- In bytes
desiredSize :: Int
desiredSize = 100 * 1024 -- 100 KiB

-- Resize and resize a poster
-- The result must be
-- 1. Small: < desired size
-- 2. less high than the desired height
-- 3. less wide than the desired width
--
-- If the input already matches these constraints, we just keep it as-is If it
-- doesn't, we try to resize it by scaling accourding to its original ratio,
-- until it is small enough.
posterCropImage :: Text -> ByteString -> Either String (Text, ByteString)
posterCropImage imageType contents = do
  -- Definitely load the image, so we are sure it's indeed an image
  (wasJpegAlready, dynamicImage) <- case imageType of
    "image/png" -> (,) False <$> decodePng contents
    "image/jpeg" -> (,) True <$> decodeJpeg contents
    "image/jpg" -> (,) True <$> decodeJpeg contents
    _ -> (,) False <$> decodeImage contents

  -- If it was a jpeg file already and is already small enough, don't do
  -- anything with it so this function is idempotent.
  -- If we would know about the encoded quality of the image then we could
  -- check that the quality is low enough, but there's no way to know the
  -- quality at which a jpeg was encoded, it seems.
  if wasJpegAlready && SB.length contents <= desiredSize
    then pure ("image/jpeg", contents)
    else do
      -- Convert to a very general jpeg format
      let jpegImage = dynamicImageToYCbCr8 dynamicImage

      -- We don't technically need to resize the image but because we know that
      -- we'll encode the image at a lower quality later, we will resize it to
      -- retain as much of the quality as we can.
      let w = someJpgWidth jpegImage :: Int
          h = someJpgHeight jpegImage :: Int
      convertedImage <- case computeNewDimensions (w, h) of
        Nothing -> reduceUntilSmallEnough jpegImage
        Just (actualWidth, actualHeight) ->
          let convertedImage = scaleSomeJpg actualWidth actualHeight jpegImage
           in reduceUntilSmallEnough convertedImage

      pure ("image/jpeg", convertedImage)

componentToLDR :: Float -> Word8
componentToLDR = truncate . (255 *) . min 1.0 . max 0.0

toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef = pixelMap pixelConverter
  where
    pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
      where
        r = componentToLDR rf
        g = componentToLDR gf
        b = componentToLDR bf

greyScaleToStandardDef :: Image PixelF -> Image Pixel8
greyScaleToStandardDef = pixelMap componentToLDR

from16to8 ::
  ( PixelBaseComponent source ~ Word16,
    PixelBaseComponent dest ~ Word8
  ) =>
  Image source ->
  Image dest
from16to8
  Image
    { imageWidth = w,
      imageHeight = h,
      imageData = arr
    } = Image w h transformed
    where
      transformed = VS.map toWord8 arr
      toWord8 v = fromIntegral (v `unsafeShiftR` 8)

from32to8 ::
  ( PixelBaseComponent source ~ Word32,
    PixelBaseComponent dest ~ Word8
  ) =>
  Image source ->
  Image dest
from32to8
  Image
    { imageWidth = w,
      imageHeight = h,
      imageData = arr
    } = Image w h transformed
    where
      transformed = VS.map toWord8 arr
      toWord8 v = fromIntegral (v `unsafeShiftR` 24)

data SomeJpg = forall px. JpgEncodable px => SomeJpg (Image px)

scaleSomeJpg :: Int -> Int -> SomeJpg -> SomeJpg
scaleSomeJpg newWidth newHeight (SomeJpg i) = SomeJpg $ scaleBilinear newWidth newHeight i

someJpgWidth :: SomeJpg -> Int
someJpgWidth (SomeJpg i) = imageWidth i

someJpgHeight :: SomeJpg -> Int
someJpgHeight (SomeJpg i) = imageHeight i

dynamicImageToYCbCr8 :: DynamicImage -> SomeJpg
dynamicImageToYCbCr8 = go
  where
    go = \case
      ImageYCbCr8 img -> SomeJpg img
      ImageCMYK8 img -> SomeJpg img
      ImageCMYK16 img -> go . ImageRGB16 $ convertImage img
      ImageRGB8 img -> SomeJpg img
      ImageRGBF img -> go . ImageRGB8 $ toStandardDef img
      ImageRGBA8 img -> go $ ImageRGB8 $ dropAlphaLayer img
      ImageYF img -> go . ImageY8 $ greyScaleToStandardDef img
      ImageY8 img -> SomeJpg img
      ImageYA8 img -> go $ ImageY8 $ dropAlphaLayer img
      ImageY16 img -> go . ImageY8 $ from16to8 img
      ImageYA16 img -> go . ImageYA8 $ from16to8 img
      ImageY32 img -> go . ImageY8 $ from32to8 img
      ImageRGB16 img -> go . ImageRGB8 $ from16to8 img
      ImageRGBA16 img -> go . ImageRGBA8 $ from16to8 img

data Orientation = Landscape | Square | Portrait

computeOrientation :: (Int, Int) -> Orientation
computeOrientation (w, h) = case compare w h of
  LT -> Portrait
  EQ -> Square
  GT -> Landscape

-- Nothing means don't rescale
computeNewDimensions :: (Int, Int) -> Maybe (Int, Int)
computeNewDimensions (w, h) =
  case computeOrientation (w, h) of
    Portrait -> rescaleToDesired (desiredPortraitWidth, desiredPortraitHeight) (w, h)
    _ -> rescaleToDesired (desiredLandscapeWidth, desiredLandscapeHeight) (w, h)

rescaleToDesired :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
rescaleToDesired (desiredWidth, desiredHeight) (w, h) =
  if w <= desiredWidth && h <= desiredHeight
    then -- We don't want to upsize, only downsize.
      Nothing
    else Just $
      case compare (w % h) (desiredWidth % desiredHeight) of
        -- If the real ratio is less than the desired ratio, it's more portrait than the desired ratio.
        -- In that case we want the height to be equal to the desired height (less than the current height)
        -- and the width to be adjusted downward while keeping the image ratio.
        LT ->
          let h' = desiredHeight
              w' = rescaleWidth h' w h
           in (w', h')
        -- If the real ratio is greater than the desired ratio, it's more landscape than the desired ratio.
        -- In that case we want the width to be equal to the desired width (less than the current width)
        -- and the height to be adjusted downward while keeping the image ratio.
        _ ->
          let w' = desiredWidth
              h' = rescaleHeight w' h w
           in (w', h')

-- | Rescale the width given a new height, retaining the ratio of width to height
--
-- we want:
--
--  w / h == w' / h'
--
-- h' = desiredHeight
-- w' = desiredHeight * w / h
--
-- This works:
--
--  w' / h' == (desiredHeight * w / h) / desiredHeight
--          == w / h
rescaleWidth :: Int -> Int -> Int -> Int
rescaleWidth newHeight oldWidth oldHeight = round $ (fromIntegral newHeight * fromIntegral oldWidth) / (fromIntegral oldHeight :: Float)

-- | Rescale the height given a new width, retaining the ratio of width to height
--
-- we want:
--
-- w / h == w' / h'
--
-- w' = desiredWidth
-- h' = desiredWidth * h / w
--
-- This works:
--
-- w' / h' == desiredWidth / (desiredWidth * h / w)
--         == desiredWidth * w / desiredWidth * h
--         == w / h
rescaleHeight :: Int -> Int -> Int -> Int
rescaleHeight newWidth oldHeight oldWidth = round $ (fromIntegral newWidth * fromIntegral oldHeight) / (fromIntegral oldWidth :: Float)

reduceUntilSmallEnough :: SomeJpg -> Either String ByteString
reduceUntilSmallEnough (SomeJpg image) = go startingQuality
  where
    go currentQuality
      | currentQuality > 0 =
        let candidate = LB.toStrict $ encodeDirectJpegAtQualityWithMetadata currentQuality metas image
         in if SB.length candidate <= desiredSize
              then pure candidate
              else go $ currentQuality - stepSize
      | otherwise = Left "Failed to produce a small enough image by reducing the size"
    stepSize = 5

    metas :: Metadatas
    metas = mempty

    -- Starting quality, between 0 and 100
    -- It makes sense to start at 100% because jpeg is already smaller than png, usually
    startingQuality = 80
