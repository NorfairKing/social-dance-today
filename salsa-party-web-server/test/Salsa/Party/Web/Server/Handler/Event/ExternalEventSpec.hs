{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEventSpec (spec) where

import Data.Aeson as JSON
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import Web.JSONLD as LD

spec :: Spec
spec = do
  serverSpec $ do
    describe "EventR" $ do
      it "Can get the party page for an existing external event" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              get $ EventR $ externalEventUuid externalEvent
              statusIs 200
      it "Can get the party page for an existing party in application/ld+json format" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              request $ do
                setUrl $ EventR $ externalEventUuid externalEvent
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
