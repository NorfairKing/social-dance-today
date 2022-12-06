{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Salsa.Party.Poster
  ( desiredWidth,
    desiredHeight,
    desiredSize,
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

-- In pixels
desiredWidth :: Int
desiredWidth = 640

-- In pixels
desiredHeight :: Int
desiredHeight = 360

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
  -- Load the image
  dynamicImage <- case imageType of
    "image/png" -> decodePng contents
    "image/jpeg" -> decodeJpeg contents
    "image/jpg" -> decodeJpeg contents
    _ -> decodeImage contents

  -- Convert to a very general jpeg format
  let jpegImage = dynamicImageToYCbCr8 dynamicImage

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

-- Nothing means don't rescale
computeNewDimensions :: (Int, Int) -> Maybe (Int, Int)
computeNewDimensions (w, h) =
  if w <= desiredWidth && h <= desiredHeight
    then -- We don't want to upsize, only downsize.
      Nothing
    else Just $
      case compare (w % h) (desiredWidth % desiredHeight) of
        LT ->
          -- If the real ratio is less than the desired ratio, it's more portrait than the desired ratio.
          -- In that case we want the height to be equal to the desired height (less than the current height)
          -- and the width to be adjusted downward while keeping the image ratio.
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
          let h' = desiredHeight
              w' = round $ (fromIntegral desiredHeight * fromIntegral w) / (fromIntegral h :: Float)
           in (w', h')
        _ ->
          -- If the real ratio is greater than the desired ratio, it's more landscape than the desired ratio.
          -- In that case we want the width to be equal to the desired width (less than the current width)
          -- and the height to be adjusted downward while keeping the image ratio.
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
          let w' = desiredWidth
              h' = round $ (fromIntegral desiredWidth * fromIntegral h) / (fromIntegral w :: Float)
           in (w', h')

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
