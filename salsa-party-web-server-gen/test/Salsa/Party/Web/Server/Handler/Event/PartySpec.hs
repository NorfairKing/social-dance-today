{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.PartySpec (spec) where

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
              _ <- followRedirect -- We may end up on a slug-based route, but that's fine.
              statusIs 200

    it "Can get the party page for an existing party by slugs" $ \yc ->
      forAllValid $ \organiser ->
        forAllValid $ \place ->
          forAllValid $ \party ->
            case (,) <$> organiserSlug organiser <*> partySlug party of
              Nothing -> pure () -- Don't care if they don't both have slugs.
              Just (organiserSlug_, partySlug_) -> do
                runYesodClientM yc $ do
                  testDB $ do
                    organiserId <- DB.insert organiser
                    placeId <- DB.insert place
                    DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
                  get $ PartySlugR organiserSlug_ partySlug_ (partyDay party)
                  statusIs 200
