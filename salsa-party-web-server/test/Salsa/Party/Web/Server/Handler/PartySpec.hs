{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import qualified Database.Persist as DB
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
                poster <- readTestFile "static/locations/Sydney.jpg"
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

  describe "PosterR" $ do
    it "GETS a 404 for a nonexistent poster" $ \yc -> do
      forAllValid $ \casKey ->
        runYesodClientM yc $ do
          get $ PosterR casKey
          statusIs 404

    it "Can GET the poster for a party with a poster" $ \yc ->
      forAllValid $ \image -> runYesodClientM yc $ do
        testDB $ DB.insert_ image
        get $ PosterR $ imageKey image
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

  describe "DeleteAccountPartyR" $
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
