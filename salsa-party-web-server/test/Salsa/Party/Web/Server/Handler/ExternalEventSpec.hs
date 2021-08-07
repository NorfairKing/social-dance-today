{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.ExternalEventSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

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
