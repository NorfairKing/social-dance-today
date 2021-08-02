{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.ScheduleSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "AccountSchedulesR" $ do
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

  describe "AccountSubmitScheduleR" $ do
    it "GETS a 200 by GETing SubmitScheduleR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          get $ AccountR AccountSubmitScheduleR
          statusIs 200

    it "Can create a schedule by POSTing to SubmitScheduleR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \scheduleForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              scheduleUuid_ <-
                testAddSchedule
                  scheduleForm_
                  location
              verifyScheduleAdded scheduleUuid_ scheduleForm_

    it "Can create a schedule with a poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \scheduleForm_ ->
          forAllValid $ \location -> do
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              poster_ <- readTestFile "test_resources/posters/1.png"
              scheduleUuid_ <-
                testAddScheduleWithPoster
                  scheduleForm_
                  location
                  poster_
              verifyScheduleAddedWithPoster scheduleUuid_ scheduleForm_ poster_

  describe "AccountScheduleR" $ do
    pending "cannot GET a nonexistent schedule"
    pending "can GET an existent schedule"
    pending "cannot GET another organiser's schedule"
    pending "can edit an existing schedule"
    pending "can edit an existing schedule's poster"
    pending "cannot edit a nonexisting schedule"
    pending "cannot edit another nonexisting schedule"
    pending "cannot edit another organiser's schedule"
