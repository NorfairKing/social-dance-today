{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "SubmitPartyR" $ do
    it "GETs a 200 for SubmitPartyR" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get SubmitPartyR
        statusIs 200
    it "Can create a party by POSTing to SubmitPartyR" $ \yc ->
      forAllValid $ \partyForm_ ->
        forAllValid $ \location ->
          withAnyLoggedInUser_ yc $
            void $
              testSubmitParty
                partyForm_
                location
    it "Can get the party page for an existing party" $ \yc ->
      forAllValid $ \partyForm_ ->
        forAllValid $ \location ->
          withAnyLoggedInUser_ yc $ do
            partyId <-
              testSubmitParty
                partyForm_
                location
            get $ PartyR partyId
            statusIs 200

  describe "PartyR" $
    yit "GETs a 404 for a nonexistent party" $ do
      get $ PartyR $ toSqlKey 666
      statusIs 404
