{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSONSpec (spec) where

import Data.Aeson as JSON
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Web.JSONLD as LD
import Yesod.Core

spec :: Spec
spec = serverSpec $ do
  describe "EventR" $ do
    it "Can get the json ld for an existing external event via an accept header" $ \yc ->
      forAllValid $ \place ->
        forAllValid $ \externalEvent ->
          forAllValid $ \importerMetadata ->
            case externalEventSlugRoute externalEvent of
              Nothing -> pure ()
              Just route ->
                runYesodClientM yc $
                  withLoggedInAdmin $ do
                    testDB $ do
                      placeId <- DB.insert place
                      importerMetadataId <- DB.insert importerMetadata
                      DB.insert_ $ externalEvent {externalEventPlace = placeId, externalEventImporter = importerMetadataId}
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
