{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Account.ScheduleSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Looper.PartyScheduler
import Salsa.Party.Web.Server.Handler.Account.Schedule
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

    it "can create this example schedule and have the parties created immediately" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \scheduleFormPrototype_ ->
          forAllValid $ \location -> do
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              let scheduleForm_ = scheduleFormPrototype_ {addScheduleFormRecurrence = WeeklyRecurrence Monday}
              scheduleUuid_ <- testAddSchedule scheduleForm_ location
              verifyScheduleAdded scheduleUuid_ scheduleForm_
              mScheduleId_ <- testDB $ DB.selectFirst [] []
              case mScheduleId_ of
                Nothing -> liftIO $ expectationFailure "Should have found a schedule"
                Just (Entity scheduleId_ _) -> do
                  parties <- testDB $ do
                    schedulePartyRows <- DB.selectList [SchedulePartySchedule DB.==. scheduleId_] []
                    forM schedulePartyRows $ \(Entity _ ScheduleParty {..}) -> DB.get schedulePartyParty
                  liftIO $ do
                    parties `shouldSatisfy` all isJust
                    genericLength parties `shouldSatisfy` (>= (daysToScheduleAhead `div` 7))

  describe "AccountScheduleR" $ do
    it "can GET an existent schedule" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \scheduleForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              scheduleId <-
                testAddSchedule
                  scheduleForm_
                  location
              get $ AccountR $ AccountScheduleR scheduleId
              statusIs 200

    it "cannot GET a nonexistent schedule" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          uuid <- nextRandomUUID
          get $ AccountR $ AccountScheduleR uuid
          statusIs 404

    it "cannot GET another organiser's schedule" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \scheduleForm_ ->
                forAllValid $ \location -> runYesodClientM yc $ do
                  scheduleId <- asNewUser testUser1 $ do
                    testSubmitOrganiser organiser1Form_
                    testAddSchedule scheduleForm_ location
                  asNewUser testUser2 $ do
                    testSubmitOrganiser organiser2Form_
                    get $ AccountR $ AccountScheduleR scheduleId
                    statusIs 403

    it "can edit an existing schedule" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \addScheduleForm_ ->
          forAllValid $ \editScheduleForm_ ->
            forAllValid $ \location ->
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                scheduleUuid_ <-
                  testAddSchedule
                    addScheduleForm_
                    location
                get $ AccountR $ AccountScheduleR scheduleUuid_
                statusIs 200
                testEditSchedule scheduleUuid_ editScheduleForm_ location
                statusIs 303
                _ <- followRedirect
                statusIs 200
                verifyScheduleEdited scheduleUuid_ editScheduleForm_

    it "can update this example schedule and have its future parties updated automatically" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \addScheduleForm_ ->
          forAllValid $ \editScheduleForm_ ->
            forAllValid $ \location ->
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                scheduleUuid_ <-
                  testAddSchedule
                    addScheduleForm_
                    location
                mScheduleId_ <- testDB $ DB.selectFirst [] []
                partyIds <- case mScheduleId_ of
                  Nothing -> liftIO $ expectationFailure "Should have found a schedule"
                  Just (Entity scheduleId_ _) -> testDB $ map (schedulePartyParty . entityVal) <$> DB.selectList [SchedulePartySchedule DB.==. scheduleId_] []
                partiesBefore <- testDB $ fmap catMaybes $ mapM DB.get partyIds
                forM_ partiesBefore $ \partyBefore ->
                  verifyScheduleAddedParty (partyUuid partyBefore) addScheduleForm_
                get $ AccountR $ AccountScheduleR scheduleUuid_
                statusIs 200
                testEditSchedule scheduleUuid_ editScheduleForm_ location
                statusIs 303
                _ <- followRedirect
                statusIs 200
                partiesAfter <- testDB $ fmap catMaybes $ mapM DB.get partyIds
                forM_ partiesAfter $ \partyAfter ->
                  verifyScheduleEditedParty (partyUuid partyAfter) editScheduleForm_

    it "can edit an existing schedule's poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \addScheduleForm_ ->
          forAllValid $ \editScheduleForm_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster1 <- readTestFile "test_resources/posters/1.png"
                poster2 <- readTestFile "test_resources/posters/2.png"
                scheduleUuid_ <-
                  testAddScheduleWithPoster
                    addScheduleForm_
                    location
                    poster1
                mSchedule <- testDB $ DB.getBy $ UniqueScheduleUUID scheduleUuid_
                mCasKey1 <- case mSchedule of
                  Nothing -> liftIO $ expectationFailure "expected the first schedule to exist."
                  Just (Entity scheduleId _) -> testDB $ getPosterForSchedule scheduleId
                -- There is now a poster.
                liftIO $ mCasKey1 `shouldBe` testFileCASKey poster1
                get $ AccountR $ AccountScheduleR scheduleUuid_
                statusIs 200
                testEditScheduleWithPoster scheduleUuid_ editScheduleForm_ location poster2
                statusIs 303
                _ <- followRedirect
                statusIs 200
                verifyScheduleEditedWithPoster scheduleUuid_ editScheduleForm_ poster2

    it "can update this example schedule and have its future parties' poster updated automatically" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \addScheduleForm_ ->
          forAllValid $ \editScheduleForm_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster1 <- readTestFile "test_resources/posters/1.png"
                poster2 <- readTestFile "test_resources/posters/2.png"
                scheduleUuid_ <-
                  testAddScheduleWithPoster
                    addScheduleForm_
                    location
                    poster1
                mScheduleId_ <- testDB $ DB.selectFirst [] []
                partyIds <- case mScheduleId_ of
                  Nothing -> liftIO $ expectationFailure "Should have found a schedule"
                  Just (Entity scheduleId_ _) -> testDB $ map (schedulePartyParty . entityVal) <$> DB.selectList [SchedulePartySchedule DB.==. scheduleId_] []
                partiesBefore <- testDB $ fmap catMaybes $ mapM DB.get partyIds
                forM_ partiesBefore $ \partyBefore -> do
                  verifyScheduleAddedPartyWithPoster (partyUuid partyBefore) addScheduleForm_ poster1
                get $ AccountR $ AccountScheduleR scheduleUuid_
                statusIs 200
                testEditScheduleWithPoster scheduleUuid_ editScheduleForm_ location poster2
                statusIs 303
                _ <- followRedirect
                statusIs 200
                partiesAfter <- testDB $ fmap catMaybes $ mapM DB.get partyIds
                forM_ partiesAfter $ \partyAfter -> do
                  verifyScheduleEditedPartyWithPoster (partyUuid partyAfter) editScheduleForm_ poster2

    it "cannot edit a nonexisting schedule" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \scheduleForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              get $ AccountR AccountSubmitScheduleR
              statusIs 200
              uuid <- nextRandomUUID
              testEditSchedule uuid scheduleForm_ location
              statusIs 404

    it "cannot edit another organiser's schedule" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \addScheduleForm_ ->
                forAllValid $ \editScheduleForm_ ->
                  forAllValid $ \location -> runYesodClientM yc $ do
                    scheduleUuid_ <- asNewUser testUser1 $ do
                      testLoginUser testUser1
                      testSubmitOrganiser organiser1Form_
                      testAddSchedule addScheduleForm_ location
                    asNewUser testUser2 $ do
                      testSubmitOrganiser organiser2Form_
                      testEditSchedule scheduleUuid_ editScheduleForm_ location
                      statusIs 403

  describe "AccountScheduleDeleteR" $ do
    it "can delete a schedule" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \scheduleForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              scheduleId <-
                testAddSchedule
                  scheduleForm_
                  location
              get $ AccountR $ AccountScheduleR scheduleId
              statusIs 200
              request $ do
                setMethod methodPost
                setUrl $ AccountR $ AccountScheduleDeleteR scheduleId
                addToken
              statusIs 303
              locationShouldBe $ AccountR AccountSchedulesR
              _ <- followRedirect
              statusIs 200
              mSchedule <- testDB (DB.getBy (UniqueScheduleUUID scheduleId))
              liftIO $ mSchedule `shouldBe` Nothing

    it "cannot delete another user's schedule" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiserForm_ ->
            forAllValid $ \scheduleForm_ ->
              forAllValid $ \location -> runYesodClientM yc $ do
                scheduleId <- asNewUser testUser1 $ do
                  testSubmitOrganiser organiserForm_
                  testAddSchedule scheduleForm_ location
                asNewUser testUser2 $ do
                  request $ do
                    setMethod methodPost
                    setUrl $ AccountR $ AccountScheduleDeleteR scheduleId
                    addToken
                  statusIs 403

    it "cannot delete a nonexistent schedule" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountOverviewR
        statusIs 200
        uuid <- nextRandomUUID
        request $ do
          setMethod methodPost
          setUrl $ AccountR $ AccountScheduleDeleteR uuid
          addToken
        statusIs 404
