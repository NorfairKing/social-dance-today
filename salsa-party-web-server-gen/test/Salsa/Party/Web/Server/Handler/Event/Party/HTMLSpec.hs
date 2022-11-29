{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.Party.HTMLSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "EventR" $ do
    it "GETs a 404 or 410 for a nonexistent party by uuid" $ \yc ->
      forAllValid $ \uuid -> runYesodClientM yc $ do
        get $ EventR uuid
        s <- requireStatus
        case s of
          404 -> pure ()
          410 -> pure ()
          _ -> liftIO $ expectationFailure $ show s

    it "Can get the party page for an existing party by uuid" $ \yc ->
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
              shouldHaveNoNoArchiveXRobotsTag
              shouldHaveNoUnavailableAfterXRobotsTag

    it "GETs a 404 or 410 for a nonexistent party by slugs" $ \yc ->
      forAllValid $ \organiserSlug_ ->
        forAllValid $ \partySlug_ ->
          forAllValid $ \day ->
            runYesodClientM yc $ do
              get $ PartySlugR organiserSlug_ partySlug_ day
              s <- requireStatus
              case s of
                404 -> pure ()
                410 -> pure ()
                _ -> liftIO $ expectationFailure $ show s

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
                  shouldHaveNoNoArchiveXRobotsTag
                  shouldHaveNoUnavailableAfterXRobotsTag
