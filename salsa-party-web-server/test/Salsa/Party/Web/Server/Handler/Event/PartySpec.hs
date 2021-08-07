{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.PartySpec (spec) where

import Data.Aeson as JSON
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import Web.JSONLD as LD

spec :: Spec
spec = serverSpec $ do
  describe "EventR" $ do
    yit "GETs a 404 for a nonexistent party" $ do
      uuid <- nextRandomUUID
      get $ EventR uuid
      statusIs 404

    it "Can get the party page for an existing party" $ \yc ->
      forAllValid $ \organiser ->
        forAllValid $ \place ->
          forAllValid $ \party ->
            runYesodClientM yc $ do
              testDB $ do
                organiserId <- DB.insert organiser
                placeId <- DB.insert place
                DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
              get $ EventR $ partyUuid party
              statusIs 200

    it "Can get the party page for an existing party in application/ld+json format" $ \yc ->
      forAllValid $ \organiser ->
        forAllValid $ \place ->
          forAllValid $ \party ->
            runYesodClientM yc $ do
              testDB $ do
                organiserId <- DB.insert organiser
                placeId <- DB.insert place
                DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
              request $ do
                setUrl $ EventR $ partyUuid party
                addRequestHeader ("Accept", "application/ld+json")
              statusIs 200
              mResp <- getResponse
              case mResp of
                Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                Just resp -> do
                  let cts = responseBody resp
                  liftIO $ case JSON.eitherDecode cts of
                    Left err -> expectationFailure err
                    Right ldEvent -> shouldBeValid (ldEvent :: LD.Event)
