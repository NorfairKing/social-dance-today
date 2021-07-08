{-# LANGUAGE TypeApplications #-}

module Web.JSONLDSpec (spec) where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Web.JSONLD as LD
import Web.JSONLD.Gen ()

spec :: Spec
spec = do
  genValidSpec @Event
  jsonSpecOnValid @Event
  genValidSpec @EventLocation
  jsonSpecOnValid @EventLocation
  genValidSpec @Place
  jsonSpecOnValid @Place
  genValidSpec @PlaceAddress
  jsonSpecOnValid @PlaceAddress
  genValidSpec @PostalAddress
  jsonSpecOnValid @PostalAddress
  genValidSpec @PlaceGeo
  jsonSpecOnValid @PlaceGeo
  genValidSpec @GeoCoordinates
  jsonSpecOnValid @GeoCoordinates
  genValidSpec @EventStartDate
  jsonSpecOnValid @EventStartDate
  genValidSpec @EventEndDate
  jsonSpecOnValid @EventEndDate
  genValidSpec @Date
  jsonSpecOnValid @Date
  genValidSpec @DateTime
  jsonSpecOnValid @DateTime
  genValidSpec @EventAttendanceMode
  jsonSpecOnValid @EventAttendanceMode
  genValidSpec @EventStatus
  jsonSpecOnValid @EventStatus
  genValidSpec @EventImage
  jsonSpecOnValid @EventImage
  genValidSpec @EventOrganizer
  jsonSpecOnValid @EventOrganizer
  genValidSpec @Organization
  jsonSpecOnValid @Organization
  scenarioDirRecur "test_resources/ld/events" $ \fp ->
    it "can be parsed as a JSONLD event" $ do
      contents <- LB.readFile fp
      case JSON.eitherDecode contents of
        Left err -> expectationFailure err
        Right e -> seq (e :: LD.Event) (pure ())
