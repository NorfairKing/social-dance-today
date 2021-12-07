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
  jsonSpec @Event
  genValidSpec @EventLocation
  jsonSpec @EventLocation
  genValidSpec @Place
  jsonSpec @Place
  genValidSpec @PlaceAddress
  jsonSpec @PlaceAddress
  genValidSpec @PostalAddress
  jsonSpec @PostalAddress
  genValidSpec @PlaceGeo
  jsonSpec @PlaceGeo
  genValidSpec @GeoCoordinates
  jsonSpec @GeoCoordinates
  genValidSpec @EventStartDate
  jsonSpec @EventStartDate
  genValidSpec @EventEndDate
  jsonSpec @EventEndDate
  genValidSpec @Date
  jsonSpec @Date
  genValidSpec @DateTime
  jsonSpec @DateTime
  genValidSpec @EventAttendanceMode
  jsonSpec @EventAttendanceMode
  genValidSpec @EventStatus
  jsonSpec @EventStatus
  genValidSpec @EventImage
  jsonSpec @EventImage
  genValidSpec @EventOrganizer
  jsonSpec @EventOrganizer
  genValidSpec @Organization
  jsonSpec @Organization
  scenarioDirRecur "test_resources/ld/events" $ \fp ->
    it "can be parsed as a JSONLD event" $ do
      contents <- LB.readFile fp
      case JSON.eitherDecode contents of
        Left err -> expectationFailure err
        Right e -> seq (e :: LD.Event) (pure ())
