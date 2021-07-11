{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Account.Party
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
                testSubmitParty
                  partyForm_
                  location
              get $ AccountR AccountPartiesR
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
                testSubmitParty
                  partyForm_
                  location
              mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
              liftIO $ case mParty of
                Nothing -> expectationFailure "expected the party to still exist."
                Just (Entity _ party) -> partyForm_ `partyFormShouldMatch` party

    it "Can create two parties with the same poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm1_ ->
          forAllValid $ \partyForm2_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster <- readTestFile "test_resources/posters/1.png"
                partyUuid1 <-
                  testSubmitPartyWithPoster
                    partyForm1_
                    location
                    poster
                partyUuid2 <-
                  testSubmitPartyWithPoster
                    partyForm2_
                    location
                    poster
                mParty1 <- testDB $ DB.getBy $ UniquePartyUUID partyUuid1
                mCasKey1 <- case mParty1 of
                  Nothing -> liftIO $ expectationFailure "expected the first party to exist."
                  Just (Entity partyId party) -> do
                    liftIO $ partyForm1_ `partyFormShouldMatch` party
                    testDB $ getPosterForParty partyId
                mParty2 <- testDB $ DB.getBy $ UniquePartyUUID partyUuid2
                mCasKey2 <- case mParty2 of
                  Nothing -> liftIO $ expectationFailure "expected the second party to exist."
                  Just (Entity partyId party) -> do
                    liftIO $ partyForm2_ `partyFormShouldMatch` party
                    testDB $ getPosterForParty partyId
                liftIO $ do
                  mCasKey1 `shouldBe` mCasKey2
                  mCasKey1 `shouldBe` testFileCASKey poster
                  mCasKey2 `shouldBe` testFileCASKey poster

  describe "AccountPartyR" $ do
    it "can GET a party" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              partyId <-
                testSubmitParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyR partyId
              statusIs 200

    it "cannot GET a party that does't exist" $ \yc -> do
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_
          uuid <- nextRandomUUID
          get $ AccountR $ AccountPartyR uuid
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
                    testSubmitParty partyForm_ location
                  asNewUser testUser2 $ do
                    testSubmitOrganiser organiser2Form_
                    get $ AccountR $ AccountPartyR partyId
                    statusIs 403

    it "Cannot edit an existing party's date" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAll (genValid `suchThat` (\d -> d /= partyFormDay partyForm_)) $ \day ->
            forAllValid $ \location ->
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                partyUuid_ <-
                  testSubmitParty
                    partyForm_
                    location
                get $ AccountR $ AccountPartyR partyUuid_
                statusIs 200
                request $ do
                  partyFormRequestBuilder (partyForm_ {partyFormDay = day}) Nothing
                  setUrl $ AccountR $ AccountPartyR partyUuid_
                mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
                liftIO $ case mParty of
                  Nothing -> expectationFailure "expected the party to still exist."
                  Just (Entity _ party) -> partyForm_ `partyFormShouldMatch` party

    it "Can edit a party's poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm1_ ->
          forAllValid $ \partyForm2_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster1 <- readTestFile "test_resources/posters/1.png"
                poster2 <- readTestFile "test_resources/posters/2.png"
                partyUuid_ <-
                  testSubmitPartyWithPoster
                    partyForm1_
                    location
                    poster1
                mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
                mCasKey1 <- case mParty of
                  Nothing -> liftIO $ expectationFailure "expected the first party to exist."
                  Just (Entity partyId _) -> testDB $ getPosterForParty partyId
                -- There is now a poster.
                liftIO $ mCasKey1 `shouldBe` testFileCASKey poster1
                get $ AccountR $ AccountPartyR partyUuid_
                statusIs 200
                testDB $ insertPlace (partyFormAddress partyForm2_) location
                request $ do
                  partyFormRequestBuilder partyForm2_ (Just poster2)
                  setUrl $ AccountR $ AccountPartyR partyUuid_
                statusIs 303
                _ <- followRedirect
                statusIs 200
                mParty2 <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
                mCasKey2 <- case mParty2 of
                  Nothing -> liftIO $ expectationFailure "expected the second party to exist."
                  Just (Entity partyId _) -> testDB $ getPosterForParty partyId
                -- The poster is now different
                liftIO $ mCasKey2 `shouldBe` testFileCASKey poster2

    it "Cannot submit to a nonexistent party" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          withAnyLoggedInUser_ yc $ do
            testSubmitOrganiser organiserForm_
            get $ AccountR AccountSubmitPartyR
            statusIs 200
            uuid <- nextRandomUUID
            request $ do
              partyFormRequestBuilder partyForm_ Nothing
              setUrl $ AccountR $ AccountPartyR uuid
            statusIs 404

    it "Cannot edit another organiser's party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \partyForm1_ ->
                forAllValid $ \partyForm2_ ->
                  forAllValid $ \location -> runYesodClientM yc $ do
                    partyUuid_ <- asNewUser testUser1 $ do
                      testLoginUser testUser1
                      testSubmitOrganiser organiser1Form_
                      testSubmitParty
                        partyForm1_
                        location
                    asNewUser testUser2 $ do
                      testSubmitOrganiser organiser2Form_
                      testDB $ insertPlace (partyFormAddress partyForm2_) location
                      request $ do
                        partyFormRequestBuilder partyForm2_ Nothing
                        setUrl $ AccountR $ AccountPartyR partyUuid_
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
                    testSubmitParty partyForm_ location
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
              partyId <- testSubmitParty partyForm_ location
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
                testSubmitParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyR partyId
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
                  testSubmitParty partyForm_ location
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
                testSubmitParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyR partyId
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
                  testSubmitParty
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
                testSubmitParty
                  partyForm_
                  location
              get $ AccountR $ AccountPartyR partyId
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
                    testSubmitParty
                      partyForm_
                      location
                  get $ AccountR $ AccountPartyR partyId
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
