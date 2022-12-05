{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Poster where

import Codec.Picture
import Codec.Picture.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
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

-- Starting quality, between 0 and 100
-- It makes sense to start at 100% because jpeg is already smaller than png, usually
startingQuality :: Word8
startingQuality = 80

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
    _ -> decodeImage contents

  -- Convert to a very general jpeg format
  let jpegImage = case dynamicImage of
        ImageYCbCr8 i -> i -- No need to convert if it's already the right format.
        _ -> convertImage $ convertRGB8 dynamicImage

  convertedImage <- reduceUntilSmallEnough jpegImage
  pure ("image/jpeg", convertedImage)

reduceUntilSmallEnough :: Image PixelYCbCr8 -> Either String ByteString
reduceUntilSmallEnough image = go startingQuality
  where
    go currentQuality
      | currentQuality > 0 =
        let candidate = LB.toStrict $ encodeJpegAtQuality currentQuality image
         in if SB.length candidate <= desiredSize
              then pure candidate
              else go $ currentQuality - stepSize
      | otherwise = Left "Failed to produce a small enough image by reducing the size"
    stepSize = 5
