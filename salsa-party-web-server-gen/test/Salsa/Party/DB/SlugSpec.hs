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

        it "is idempotent" $
          forAllValid $ \text ->
            case mkSlug text of
              Nothing -> pure ()
              Just slug1 -> case mkSlug (unSlug slug1) of
                Nothing -> expectationFailure "Should have been able to slug an existing slug"
                Just slug2 -> slug2 `shouldBe` slug1

        it "deals with these fancy diacritics" $
          -- https://twitter.com/alittlelisper/status/1438411509328084994
          case mkSlug "aăâáảàãạắẳằẵặấẩầẫậ-oóỏòõọôốổồỗộơóởờỡợ-uưúứủửùừũừụự-eêéếẻểèềẽễẹệ-iíỉìĩị" of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "aaaaaaaaaaaaaaaaaa-oooooooooooooooooo-uuuuuuuuuuuu-eeeeeeeeeeee-iiiiii"

        it "deals with these german letters" $
          case mkSlug "ß" of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "ss"

        it "produces a nice slug for this title" $
          case mkSlug "Bachata Community Zürich Mondays 💃🕺" of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "bachata-community-zurich-mondays"

        it "produces a nice slug for this title" $
          case mkSlug "🌟 NOCHE LATINA 🌟  Salsa On2 Workshop und  Party mit DJ Raffi " of
            Nothing -> expectationFailure "Should have made a valid slug"
            Just s -> do
              shouldBeValid s
              s `shouldBe` Slug "noche-latina-salsa-on2-workshop-und-party-mit-dj-raffi"
