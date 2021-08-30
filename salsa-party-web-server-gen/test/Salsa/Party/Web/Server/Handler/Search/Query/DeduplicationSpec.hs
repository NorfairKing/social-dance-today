{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Search.Query.DeduplicationSpec (spec) where

import qualified Data.Text.IO as T
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  describe "descriptionCloseEnoughTo" $ do
    it "considers these salsavida descriptions equal." $ do
      t1 <- T.readFile "test_resources/deduplication/description/1a.txt"
      t2 <- T.readFile "test_resources/deduplication/description/1b.txt"
      unless (descriptionCloseEnoughTo (Just t1) (Just t2)) $ expectationFailure "Should have been considered close enough."
    it "does not consider two Nothings equal" $
      not $ descriptionCloseEnoughTo Nothing Nothing
    it "does not consider two empty descriptions equal" $
      not $ descriptionCloseEnoughTo (Just "") (Just "")
  describe "placeCloseEnough" $ do
    it "Comma in address" $
      placeCloseEnoughTo
        "Viaduktstrasse 67, 8005 Zürich"
        "Viaduktstrasse 67 8005 Zürich"
  describe "titleCloseEnoughTo" $ do
    it "Zouk party" $
      titleCloseEnoughTo
        "Zouk meets Bachata @Bürkliplatz 😊🎵"
        "ZOUK meets BACHATA Party @Bürkliplatz 😊🎵"
    it "Extra letter somewhere" $
      titleCloseEnoughTo
        "Bachata Community Zürich Monday 💃🕺"
        "Bachata Community Zürich Mondays 💃🕺"
    it "Different casing" $
      titleCloseEnoughTo
        "Noche Latina mit Powell und DJ Ñoño"
        "NOCHE LATINA - mit Powell und DJ Ñoño"
    it "Extra nonsense" $
      titleCloseEnoughTo
        "Bachateros Treff"
        "BACHATEROS TREFF ★★★★★"
    it "Completely different" $
      not $
        titleCloseEnoughTo
          "Bachata Community Zürich Monday 💃🕺"
          "Lounge@Bananenreiferei"
    it "Completely different" $
      not $
        titleCloseEnoughTo
          "Social Salsa Party"
          "Free workshops!"
    it "Close but different" $
      not $
        titleCloseEnoughTo
          "Syd's birthday party"
          "Josh's birthday party"
    it "Completely different but all symbols" $
      not $
        titleCloseEnoughTo
          "平仮名"
          "漢字"
