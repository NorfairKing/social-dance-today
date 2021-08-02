{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.ScheduleSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  it "GETS a 303 for any account without any organiser" $ \yc -> do
    withAnyLoggedInUser_ yc $ do
      get $ AccountR AccountSchedulesR
      statusIs 303
      locationShouldBe $ AccountR AccountOrganiserR
      _ <- followRedirect
      statusIs 200
  it "GETS a 200 for any account without any schedules" $ \yc -> do
    forAllValid $ \organiserForm_ ->
      withAnyLoggedInUser_ yc $ do
        testSubmitOrganiser organiserForm_
        get $ AccountR AccountSchedulesR
        statusIs 200
  it "GETS a 200 for any account with a schedule" $ \yc -> do
    forAllValid $ \organiserForm_ ->
      forAllValid $ \partyForm_ ->
        forAllValid $ \location ->
          withAnyLoggedInUser_ yc $ do
            testSubmitOrganiser organiserForm_
            _ <-
              testAddParty
                partyForm_
                location
            get $ AccountR AccountSchedulesR
            statusIs 200
