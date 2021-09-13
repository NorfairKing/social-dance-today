{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.SlugSpec (spec) where

import Data.Fixed
import Salsa.Party.DB.Slug
import Salsa.Party.Web.Server.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist
import Text.Printf
import Web.PathPieces

spec :: Spec
spec = do
  genValidSpec @Slug
  modifyMaxSize (* 10) $
    modifyMaxSuccess (* 10) $
      describe "mkSlug" $ do
        it "produces valid slugs" $
          producesValidsOnValids mkSlug

        it "is idemptent" $
          forAllValid $ \text ->
            case mkSlug text of
              Nothing -> pure ()
              Just slug1 -> case mkSlug (unSlug slug1) of
                Nothing -> expectationFailure "Should have been able to slug an existing slug"
                Just slug2 -> slug2 `shouldBe` slug1

        it "produces a nice slug for this title" $ do
          let Just s = mkSlug "Bachata Community Zürich Mondays 💃🕺"
          shouldBeValid s
          s `shouldBe` Slug "bachata-community-zurich-mondays"

        it "deals with these fancy diacritics" $ do
          let Just s = mkSlug "áąä éęë íįï ǫóö úųü"
          shouldBeValid s
          s `shouldBe` Slug "aaa-eee-iii-ooo-uuu"

        it "deals with these german letters" $ do
          let Just s = mkSlug "ß"
          shouldBeValid s
          s `shouldBe` Slug "ss"
