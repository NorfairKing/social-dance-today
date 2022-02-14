{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Organiser.HTMLSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "OrganiserR" $ do
    it "GETs a 404 for nonexistent organiser" $ \yc ->
      runYesodClientM yc $ do
        uuid <- nextRandomUUID
        get $ OrganiserR uuid
        statusIs 404

    it "GETs a 200 for an existent organiser with some parties" $ \yc ->
      forAllValid $ \organiser ->
        forAll (NE.toList <$> genNonEmptyOf genValid) $ \places ->
          forAll (genListOf genValid >>= mapM (\party -> (,) party <$> elements places)) $ \partyTups ->
            runYesodClientM yc $ do
              testDB $ do
                organiserId <- DB.insert organiser
                forM_ places $ \place ->
                  DB.upsertBy (UniquePlaceQuery $ placeQuery place) place []
                forM_ partyTups $ \(party, place) -> do
                  mPlaceEntity <- DB.getBy $ UniquePlaceQuery $ placeQuery place
                  forM_ mPlaceEntity $ \(Entity placeId _) ->
                    DB.insert_ $ party {partyPlace = placeId, partyOrganiser = organiserId}

              get $ OrganiserR $ organiserUuid organiser
              _ <- followRedirect
              statusIs 200
