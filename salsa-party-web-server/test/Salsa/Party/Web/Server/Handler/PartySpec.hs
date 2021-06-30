{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "PartyR" $ do
    yit "GETs a 404 for a nonexistent party" $ do
      uuid <- nextRandomUUID
      get $ PartyR uuid
      statusIs 404

    it "Can get the party page for an existing party" $ \yc ->
      forAllValid $ \organiser ->
        forAllValid $ \party ->
          runYesodClientM yc $ do
            testDB $ do
              organiserId <- DB.insert organiser
              DB.insert_ $ party {partyOrganiser = organiserId}
            get $ PartyR $ partyUuid party
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
