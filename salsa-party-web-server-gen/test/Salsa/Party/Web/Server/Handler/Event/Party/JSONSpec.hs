{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.Party.JSONSpec (spec) where

import Data.Aeson as JSON
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Web.JSONLD as LD
import Yesod.Core

spec :: Spec
spec = serverSpec $ do
  describe "EventR" $ do
    it "Can get the json export for an existing party via an accept header" $ \yc ->
      forAllValid $ \place ->
        forAll (genValid `suchThat` (\(organiser, party) -> isJust (partySlugRoute organiser party))) $ \(organiser, party) ->
          forAllValid $ \user ->
            case partySlugRoute organiser party of
              Nothing -> pure ()
              Just route ->
                runYesodClientM yc $
                  withLoggedInAdmin $ do
                    testDB $ do
                      placeId <- DB.insert place
                      userId <- DB.insert user
                      organiserId <- DB.insert $ organiser {organiserUser = userId}
                      DB.insert_ $ party {partyPlace = placeId, partyOrganiser = organiserId}
                    request $ do
                      setUrl route
                      addRequestHeader ("Accept", typeJson)
                    statusIs 200
                    mResp <- getResponse
                    case mResp of
                      Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                      Just resp -> do
                        let cts = responseBody resp
                        liftIO $ case JSON.eitherDecode cts of
                          Left err -> expectationFailure err
                          Right ldEvent -> shouldBeValid (ldEvent :: LD.Event)
