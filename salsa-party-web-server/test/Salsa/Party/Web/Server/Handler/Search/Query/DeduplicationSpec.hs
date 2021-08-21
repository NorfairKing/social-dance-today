{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Search.Query.DeduplicationSpec (spec) where

import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  describe "closeEnoughTo" $ do
    it "Zouk party" $
      closeEnoughTo
        "Zouk meets Bachata @Bürkliplatz 😊🎵"
        "ZOUK meets BACHATA Party @Bürkliplatz 😊🎵"
    it "Comma in address" $
      closeEnoughTo
        "Viaduktstrasse 67, 8005 Zürich"
        "Viaduktstrasse 67 8005 Zürich"
    it "Extra letter somewhere" $
      closeEnoughTo
        "Bachata Community Zürich Monday 💃🕺"
        "Bachata Community Zürich Mondays 💃🕺"
    it "Different casing" $
      closeEnoughTo
        "Noche Latina mit Powell und DJ Ñoño"
        "NOCHE LATINA - mit Powell und DJ Ñoño"
    it "Extra nonsense" $
      closeEnoughTo
        "Bachateros Treff"
        "BACHATEROS TREFF ★★★★★"
    it "Completely different" $
      not $
        closeEnoughTo
          "Bachata Community Zürich Monday 💃🕺"
          "Lounge@Bananenreiferei"
    it "Completely different" $
      not $
        closeEnoughTo
          "Social Salsa Party"
          "Free workshops!"
    it "Close but different" $
      not $
        closeEnoughTo
          "Syd's birthday party"
          "Josh's birthday party"
    it "Completely different but all symbols" $
      not $
        closeEnoughTo
          "平仮名"
          "漢字"
