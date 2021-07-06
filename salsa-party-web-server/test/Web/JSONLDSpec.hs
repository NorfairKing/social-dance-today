{-# LANGUAGE TypeApplications #-}

module Web.JSONLDSpec (spec) where

import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Web.JSONLD
import Web.JSONLD.Gen ()

spec :: Spec
spec = do
  genValidSpec @Event
  jsonSpecOnValid @Event
  genValidSpec @EventLocation
  jsonSpecOnValid @EventLocation
  genValidSpec @Place
  jsonSpecOnValid @Place
  genValidSpec @EventStartDate
  jsonSpecOnValid @EventStartDate
  genValidSpec @EventEndDate
  jsonSpecOnValid @EventEndDate
  genValidSpec @Date
  jsonSpecOnValid @Date
  eqSpecOnValid @Date
  genValidSpec @DateTime
  jsonSpecOnValid @DateTime
  eqSpecOnValid @DateTime
  genValidSpec @EventAttendanceMode
  jsonSpecOnValid @EventAttendanceMode
  genValidSpec @EventStatus
  jsonSpecOnValid @EventStatus
