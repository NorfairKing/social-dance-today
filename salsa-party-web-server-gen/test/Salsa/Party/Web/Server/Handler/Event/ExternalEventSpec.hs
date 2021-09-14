{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEventSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  serverSpec $ do
    describe "EventR" $ do
      it "Gets a 404 for a nonexistent external event" $ do
        uuid <- nextRandomUUID
        get $ EventR uuid
        statusIs 404
      it "Can get the party page for an existing external event" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              get $ EventR $ externalEventUuid externalEvent
              _ <- followRedirect
              statusIs 200
