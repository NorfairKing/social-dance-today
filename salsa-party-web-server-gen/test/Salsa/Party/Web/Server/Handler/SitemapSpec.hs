{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.SitemapSpec (spec) where

import Data.Containers.ListUtils
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "SitemapR" $ do
    yit "GETs a 200" $ do
      get SitemapR
      statusIs 200

    it "GETs a 200, even with a bunch of content" $ \yc ->
      forAll (genValid `suchThat` (distinct . map organiserUser) `suchThat` (distinct . map organiserUuid) `suchThat` (distinct . map organiserSlug)) $ \organisers ->
        forAll (genValid `suchThat` (distinct . map partyUuid)) $ \parties ->
          forAll (genValid `suchThat` (distinct . map (\ExternalEvent {..} -> (externalEventImporter, externalEventKey))) `suchThat` (distinct . map externalEventUuid)) $ \externalEvents ->
            forAll (genValid `suchThat` (distinct . map imageKey)) $ \images ->
              runYesodClientM yc $ do
                testDB $ do
                  DB.insertMany_ (organisers :: [Organiser])
                  DB.insertMany_ (parties :: [Party])
                  DB.insertMany_ (externalEvents :: [ExternalEvent])
                  DB.insertMany_ (images :: [Image])
                get SitemapR
                statusIs 200

  describe "RobotsR" $
    yit "GETs a 200" $ do
      get RobotsR
      statusIs 200

distinct :: Ord a => [a] -> Bool
distinct ls = nubOrd ls == ls
