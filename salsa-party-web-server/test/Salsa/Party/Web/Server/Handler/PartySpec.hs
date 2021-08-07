{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

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
