{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import qualified Data.Text as T
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Party
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
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
        forAllValid $ \PartyForm {..} ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              _ <- testSubmitPlace partyFormAddress location
              get $ AccountR AccountSubmitPartyR
              statusIs 200
              uuid <- nextRandomUUID
              request $ do
                setMethod methodPost
                setUrl $ AccountR AccountSubmitPartyR
                addToken
                addPostParam "uuid" $ uuidText uuid -- Nonexistent party
                addPostParam "title" partyFormTitle
                addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" partyFormDay
                addPostParam "address" partyFormAddress
              statusIs 404

    it "Cannot edit another organiser's party" $ \yc -> do
      let username1 = "testuser1@example.com"
      let password1 = "testpassword1"
      let username2 = "testuser2@example.com"
      let password2 = "testpassword2"
      forAllValid $ \organiser1Form_ ->
        forAllValid $ \organiser2Form_ ->
          forAllValid $ \partyForm_ ->
            forAllValid $ \PartyForm {..} ->
              forAllValid $ \location -> runYesodClientM yc $ do
                testRegister username1 password1
                testLogout
                testRegister username2 password2
                testLogout
                testLogin username1 password1
                testSubmitOrganiser organiser1Form_
                partyId <-
                  testSubmitParty
                    partyForm_
                    location
                testLogout
                testLogin username2 password2
                testSubmitOrganiser organiser2Form_
                _ <- testSubmitPlace partyFormAddress location
                request $ do
                  setMethod methodPost
                  setUrl $ AccountR AccountSubmitPartyR
                  addToken
                  addPostParam "uuid" $ uuidText partyId
                  addPostParam "title" partyFormTitle
                  addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" partyFormDay
                  addPostParam "address" partyFormAddress
                statusIs 403

  describe "PartyR" $ do
    yit "GETs a 404 for a nonexistent party" $ do
      uuid <- nextRandomUUID
      get $ PartyR uuid
      statusIs 404

    it "Can get the party page for an existing party" $ \yc ->
      -- TODO use a logged-out user and a insert-in-db test here.
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm_
              uuid <-
                testSubmitParty
                  partyForm_
                  location
              get $ PartyR uuid
              statusIs 200

    it "Can get the party page for an existing external event" $ \yc ->
      forAllValid $ \place ->
        forAllValid $ \externalEvent ->
          runYesodClientM yc $ do
            testDB $ do
              placeId <- DB.insert place
              DB.insert_ $ externalEvent {externalEventPlace = placeId}
            get $ PartyR $ externalEventUuid externalEvent
            statusIs 200

  describe "ImageR" $ do
    it "GETS a 404 for a nonexistent image" $ \yc -> do
      forAllValid $ \casKey ->
        runYesodClientM yc $ do
          get $ ImageR casKey
          statusIs 404

    it "Can GET the poster for an existent image" $ \yc ->
      forAllValid $ \image ->
        runYesodClientM yc $ do
          testDB $ DB.insert_ image
          get $ ImageR $ imageKey image
          statusIs 200

  describe "GetAccountPartiesR" $
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

    it "cannot GET another organiser's party" $ \yc -> do
      let username1 = "testuser1@example.com"
      let password1 = "testpassword1"
      let username2 = "testuser2@example.com"
      let password2 = "testpassword2"
      forAllValid $ \organiser1Form_ ->
        forAllValid $ \organiser2Form_ ->
          forAllValid $ \partyForm_ ->
            forAllValid $ \location -> runYesodClientM yc $ do
              testRegister username1 password1
              testLogout
              testRegister username2 password2
              testLogout
              testLogin username1 password1
              testSubmitOrganiser organiser1Form_
              partyId <-
                testSubmitParty
                  partyForm_
                  location
              testLogout
              testLogin username2 password2
              testSubmitOrganiser organiser2Form_
              get $ AccountR $ AccountPartyR partyId
              statusIs 403

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

    it "cannot delete another users' party" $ \yc -> do
      let username1 = "testuser1@example.com"
      let password1 = "testpassword1"
      let username2 = "testuser2@example.com"
      let password2 = "testpassword2"
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location -> runYesodClientM yc $ do
            testRegister username1 password1
            testLogout
            testRegister username2 password2
            testLogout
            testLogin username1 password1
            testSubmitOrganiser organiserForm_
            partyId <-
              testSubmitParty
                partyForm_
                location
            testLogout
            testLogin username2 password2
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

    it "cannot cancel another users' party" $ \yc -> do
      let username1 = "testuser1@example.com"
      let password1 = "testpassword1"
      let username2 = "testuser2@example.com"
      let password2 = "testpassword2"
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location -> runYesodClientM yc $ do
            testRegister username1 password1
            testLogout
            testRegister username2 password2
            testLogout
            testLogin username1 password1
            testSubmitOrganiser organiserForm_
            partyId <-
              testSubmitParty
                partyForm_
                location
            testLogout
            testLogin username2 password2
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

    it "cannot un-cancel another users' party" $ \yc -> do
      let username1 = "testuser1@example.com"
      let password1 = "testpassword1"
      let username2 = "testuser2@example.com"
      let password2 = "testpassword2"
      forAllValid $ \organiserForm_ ->
        forAllValid $ \partyForm_ ->
          forAllValid $ \location -> runYesodClientM yc $ do
            testRegister username1 password1
            testLogout
            testRegister username2 password2
            testLogout
            testLogin username1 password1
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
            testLogout
            testLogin username2 password2
            request $ do
              setMethod methodPost
              setUrl $ AccountR $ AccountPartyCancelR partyId
              addToken
            statusIs 403
