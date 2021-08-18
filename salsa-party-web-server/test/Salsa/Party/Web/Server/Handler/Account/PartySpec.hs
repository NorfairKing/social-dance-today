{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "GetAccountPartiesR" $ do
    it "GETS a 303 for any account without any organiser" $ \yc -> do
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountPartiesR
        statusIs 303
        locationShouldBe $ AccountR AccountOrganiserR
        _ <- followRedirect
        statusIs 200

    it "GETS a 200 for any account without any parties" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          get $ AccountR AccountPartiesR
          statusIs 200

    it "GETS a 200 for any account with a party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              _ <-
                testAddParty
                  partyForm_
                  location
              get $ AccountR AccountPartiesR
              statusIs 200

  describe "AccountSubmitR" $ do
    it "GETs a 200 for AccountSubmitR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          get $ AccountR AccountSubmitR
          statusIs 200

  describe "SubmitPartyR" $ do
    it "GETs a 200 for SubmitPartyR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          get $ AccountR AccountSubmitPartyR
          statusIs 200

    it "Can create a party by POSTing to SubmitPartyR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyUuid_ <-
                testAddParty
                  partyForm_
                  location
              verifyPartyAdded partyUuid_ partyForm_

    it "Can create a party with a poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location -> do
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              poster_ <- readTestFile "test_resources/posters/1.png"
              partyUuid_ <-
                testAddPartyWithPoster
                  partyForm_
                  location
                  poster_
              verifyPartyAddedWithPoster partyUuid_ partyForm_ poster_

    it "Can create two parties with the same poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm1_ ->
          forAllValid $ \partyForm2_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster <- readTestFile "test_resources/posters/1.png"
                partyUuid1 <-
                  testAddPartyWithPoster
                    partyForm1_
                    location
                    poster
                partyUuid2 <-
                  testAddPartyWithPoster
                    partyForm2_
                    location
                    poster
                verifyPartyAddedWithPoster partyUuid1 partyForm1_ poster
                verifyPartyAddedWithPoster partyUuid2 partyForm2_ poster

  describe "AccountPartyEditR" $ do
    it "can GET a party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyId <-
                testAddParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyEditR partyId
              statusIs 200

    it "cannot GET a party that does't exist" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          uuid <- nextRandomUUID
          get $ AccountR $ AccountPartyEditR uuid
          statusIs 404

    it "cannot GET another organiser's party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \partyForm_ ->
                forAllValid $ \location -> runYesodClientM yc $ do
                  partyId <- asNewUser testUser1 $ do
                    testSubmitOrganiser organiser1Form_
                    testAddParty partyForm_ location
                  asNewUser testUser2 $ do
                    testSubmitOrganiser organiser2Form_
                    get $ AccountR $ AccountPartyEditR partyId
                    statusIs 403

    it "Can edit an existing party" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \addPartyForm_ ->
          forAllValid $ \editPartyForm_ ->
            forAllValid $ \location ->
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                partyUuid_ <-
                  testAddParty
                    addPartyForm_
                    location
                get $ AccountR $ AccountPartyEditR partyUuid_
                statusIs 200
                testEditParty partyUuid_ editPartyForm_ location
                statusIs 303
                _ <- followRedirect
                statusIs 200
                verifyPartyEdited partyUuid_ editPartyForm_

    it "Can edit a party's poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \addPartyForm_ ->
          forAllValid $ \editPartyForm_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster1 <- readTestFile "test_resources/posters/1.png"
                poster2 <- readTestFile "test_resources/posters/2.png"
                partyUuid_ <-
                  testAddPartyWithPoster
                    addPartyForm_
                    location
                    poster1
                mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
                mCasKey1 <- case mParty of
                  Nothing -> liftIO $ expectationFailure "expected the first party to exist."
                  Just (Entity partyId _) -> testDB $ getPosterForParty partyId
                -- There is now a poster.
                liftIO $ mCasKey1 `shouldBe` testFileCASKey poster1
                get $ AccountR $ AccountPartyEditR partyUuid_
                statusIs 200
                testEditPartyWithPoster partyUuid_ editPartyForm_ location poster2
                statusIs 303
                _ <- followRedirect
                statusIs 200
                verifyPartyEditedWithPoster partyUuid_ editPartyForm_ poster2

    it "Cannot edit a nonexistent party" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              get $ AccountR AccountSubmitPartyR
              statusIs 200
              uuid <- nextRandomUUID
              testEditParty uuid partyForm_ location
              statusIs 404

    it "Cannot edit another organiser's party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \addPartyForm_ ->
                forAllValid $ \editPartyForm_ ->
                  forAllValid $ \location -> runYesodClientM yc $ do
                    partyUuid_ <- asNewUser testUser1 $ do
                      testLoginUser testUser1
                      testSubmitOrganiser organiser1Form_
                      testAddParty addPartyForm_ location
                    asNewUser testUser2 $ do
                      testSubmitOrganiser organiser2Form_
                      testEditParty partyUuid_ editPartyForm_ location
                      statusIs 403

  describe "AccountPartyDuplicateR" $ do
    it "cannot duplicate a nonexistent party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          uuid <- nextRandomUUID
          get $ AccountR $ AccountPartyDuplicateR uuid
          statusIs 404

    it "cannot duplicate another organiser's party" $ \yc -> do
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \partyForm_ ->
                forAllValid $ \location -> runYesodClientM yc $ do
                  partyId <- asNewUser testUser1 $ do
                    testSubmitOrganiser organiser1Form_
                    testAddParty partyForm_ location
                  asNewUser testUser2 $ do
                    testSubmitOrganiser organiser2Form_
                    get $ AccountR $ AccountPartyDuplicateR partyId
                    statusIs 403

    it "can duplicate an own party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyId <- testAddParty partyForm_ location
              get $ AccountR $ AccountPartyDuplicateR partyId
              statusIs 200

  describe "AccountPartyDeleteR" $ do
    it "can delete a party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyId <-
                testAddParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyEditR partyId
              statusIs 200
              request $ do
                setMethod methodPost
                setUrl $ AccountR $ AccountPartyDeleteR partyId
                addToken
              statusIs 303
              locationShouldBe $ AccountR AccountPartiesR
              _ <- followRedirect
              statusIs 200
              mParty <- testDB (DB.getBy (UniquePartyUUID partyId))
              liftIO $ mParty `shouldBe` Nothing

    it "cannot delete another user's party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiserForm_ ->
            forAllValid $ \partyForm_ ->
              forAllValid $ \location -> runYesodClientM yc $ do
                partyId <- asNewUser testUser1 $ do
                  testSubmitOrganiser organiserForm_
                  testAddParty partyForm_ location
                asNewUser testUser2 $ do
                  request $ do
                    setMethod methodPost
                    setUrl $ AccountR $ AccountPartyDeleteR partyId
                    addToken
                  statusIs 403

    it "cannot delete a nonexistent party" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountOverviewR
        statusIs 200
        uuid <- nextRandomUUID
        request $ do
          setMethod methodPost
          setUrl $ AccountR $ AccountPartyDeleteR uuid
          addToken
        statusIs 404

  describe "AccountPartyCancelR" $ do
    it "can cancel a party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyId <-
                testAddParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyEditR partyId
              statusIs 200
              request $ do
                setMethod methodPost
                setUrl $ AccountR $ AccountPartyCancelR partyId
                addToken
              statusIs 303
              locationShouldBe $ AccountR AccountPartiesR
              _ <- followRedirect
              statusIs 200
              mParty <- testDB (DB.getBy (UniquePartyUUID partyId))
              liftIO $ case mParty of
                Nothing -> expectationFailure "Should have gotten a party."
                Just (Entity _ party) -> partyCancelled party `shouldBe` True

    it "cannot cancel a party that doesn't exist." $ \yc -> do
      withAnyLoggedInUser_ yc $ do
        uuid <- nextRandomUUID
        request $ do
          setMethod methodPost
          setUrl $ AccountR $ AccountPartyCancelR uuid
          addToken
        statusIs 404

    it "cannot cancel another users' party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiserForm_ ->
            forAllValid $ \partyForm_ ->
              forAllValid $ \location -> runYesodClientM yc $ do
                partyId <- asNewUser testUser1 $ do
                  testSubmitOrganiser organiserForm_
                  testAddParty
                    partyForm_
                    location
                asNewUser testUser2 $ do
                  request $ do
                    setMethod methodPost
                    setUrl $ AccountR $ AccountPartyCancelR partyId
                    addToken
                  statusIs 403

  describe "AccountPartyUnCancelR" $ do
    it "can un-cancel a party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyId <-
                testAddParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyEditR partyId
              statusIs 200
              request $ do
                setMethod methodPost
                setUrl $ AccountR $ AccountPartyCancelR partyId
                addToken
              statusIs 303
              locationShouldBe $ AccountR AccountPartiesR
              _ <- followRedirect
              statusIs 200
              request $ do
                setMethod methodPost
                setUrl $ AccountR $ AccountPartyUnCancelR partyId
                addToken
              statusIs 303
              locationShouldBe $ AccountR AccountPartiesR
              _ <- followRedirect
              statusIs 200
              mParty <- testDB (DB.getBy (UniquePartyUUID partyId))
              liftIO $ case mParty of
                Nothing -> expectationFailure "Should have gotten a party."
                Just (Entity _ party) -> partyCancelled party `shouldBe` False

    it "cannot uncancel a party that doesn't exist." $ \yc -> do
      withAnyLoggedInUser_ yc $ do
        uuid <- nextRandomUUID
        request $ do
          setMethod methodPost
          setUrl $ AccountR $ AccountPartyUnCancelR uuid
          addToken
        statusIs 404

    it "cannot un-cancel another users' party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiserForm_ ->
            forAllValid $ \partyForm_ ->
              forAllValid $ \location -> runYesodClientM yc $ do
                partyId <- asNewUser testUser1 $ do
                  testSubmitOrganiser organiserForm_
                  partyId <-
                    testAddParty
                      partyForm_
                      location
                  get $ AccountR $ AccountPartyEditR partyId
                  statusIs 200
                  request $ do
                    setMethod methodPost
                    setUrl $ AccountR $ AccountPartyCancelR partyId
                    addToken
                  statusIs 303
                  locationShouldBe $ AccountR AccountPartiesR
                  _ <- followRedirect
                  statusIs 200
                  pure partyId
                asNewUser testUser2 $ do
                  request $ do
                    setMethod methodPost
                    setUrl $ AccountR $ AccountPartyCancelR partyId
                    addToken
                  statusIs 403
