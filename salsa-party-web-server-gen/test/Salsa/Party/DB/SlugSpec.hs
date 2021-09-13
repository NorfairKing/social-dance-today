{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.SlugSpec (spec) where

import Salsa.Party.DB
import Salsa.Party.Web.Server.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @EventSlug
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

        it "produces a nice slug for this title" $
          case mkSlug "Bachata Community Zürich Mondays 💃🕺" of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "bachata-community-zurich-mondays"

        it "deals with these fancy diacritics" $
          case mkSlug "áąä éęë íįï ǫóö úųü" of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "aaa-eee-iii-ooo-uuu"

        it "deals with these german letters" $
          case mkSlug "ß" of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "ss"
