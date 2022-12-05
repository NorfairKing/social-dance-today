{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Poster where

import Codec.Picture
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Saving (imageToJpg)
import Codec.Picture.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Ratio
import Data.Text (Text)
import Data.Word
import Debug.Trace

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
startingQuality = 100

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
  let jpegImage = case dynamicImage of
        ImageYCbCr8 i -> i -- No need to convert if it's already the right format.
        _ -> convertImage $ convertRGB8 dynamicImage

  let w = imageWidth jpegImage :: Int
      h = imageHeight jpegImage :: Int

  convertedImage <- case computeNewDimensions (w, h) of
    Nothing -> reduceUntilSmallEnough jpegImage
    Just (actualWidth, actualHeight) ->
      let convertedImage = scaleBilinear actualWidth actualHeight jpegImage
       in reduceUntilSmallEnough convertedImage

  pure ("image/jpeg", convertedImage)

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
