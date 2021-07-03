{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import qualified Data.Text as T
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
              void $
                testSubmitParty
                  partyForm_
                  location

    it "Cannot edit an existing party's date" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAll (genValid `suchThat` (\d -> d /= partyFormDay partyForm_)) $ \day ->
            forAllValid $ \location ->
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                partyUuid <-
                  testSubmitParty
                    partyForm_
                    location
                get $ AccountR $ AccountPartyR partyUuid
                statusIs 200
                request $ do
                  partyFormRequestBuilder (partyForm_ {partyFormDay = day}) Nothing
                  addPostParam "uuid" $ uuidText partyUuid
                mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid
                liftIO $ case mParty of
                  Nothing -> expectationFailure "expected the party to still exist."
                  Just (Entity _ party) -> partyDay party `shouldBe` partyFormDay partyForm_

    it "Can create two parties with the same poster" $ \yc ->
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm1_ ->
          forAllValid $ \partyForm2_ ->
            forAllValid $ \location -> do
              withAnyLoggedInUser_ yc $ do
                testSubmitOrganiser organiserForm_
                poster <- readTestFile "test_resources/poster.png"
                void $
                  testSubmitPartyWithPoster
                    partyForm1_
                    location
                    poster
                void $
                  testSubmitPartyWithPoster
                    partyForm2_
                    location
                    poster

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
              addPostParam "uuid" $ uuidText uuid -- Nonexistent party
            statusIs 404

    it "Cannot edit another organiser's party" $ \yc ->
      forAllValid $ \testUser1 ->
        forAllValid $ \testUser2 ->
          forAllValid $ \organiser1Form_ ->
            forAllValid $ \organiser2Form_ ->
              forAllValid $ \partyForm_ ->
                forAllValid $ \PartyForm {..} ->
                  forAllValid $ \location -> runYesodClientM yc $ do
                    partyId <- asNewUser testUser1 $ do
                      testLoginUser testUser1
                      testSubmitOrganiser organiser1Form_
                      testSubmitParty
                        partyForm_
                        location
                    asNewUser testUser2 $ do
                      testSubmitOrganiser organiser2Form_
                      testDB $ insertPlace partyFormAddress location
                      request $ do
                        setMethod methodPost
                        setUrl $ AccountR AccountSubmitPartyR
                        addToken
                        addPostParam "uuid" $ uuidText partyId
                        addPostParam "title" partyFormTitle
                        addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" partyFormDay
                        addPostParam "address" partyFormAddress
                      statusIs 403

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
