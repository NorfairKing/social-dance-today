{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Importer.EnvSpec (spec) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import Salsa.Party.Importer.Env
import Test.Syd

spec :: Spec
spec = do
  describe "unescapeHTML" $ do
    -- An o fits in ASCII
    it "unescapes a utf8-encoded o" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "o")) `shouldBe` "o"
    it "unescapes a decimal HTML entity o" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#111;")) `shouldBe` "o"
    it "unescapes a hexadecimal HTML entity o" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#x6f;")) `shouldBe` "o"

    -- An < fits into ASCII but needs to be escaped either way
    it "unescapes a named html entity of <" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&lt;")) `shouldBe` "<"
    it "unescapes a decimal html entity of <" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#60;")) `shouldBe` "<"
    it "unescapes a hexadecimal html entity of <" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#x3c;")) `shouldBe` "<"

    -- An ü is and does not fit into ASCII but does fit into latin1
    it "unescapes a utf8-encoded ü" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "ü")) `shouldBe` "ü"
    it "unescapes a named HTML entity of ü" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&uuml;")) `shouldBe` "ü"
    it "unescapes a decimal HTML entity of ü" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#252;")) `shouldBe` "ü"
    it "unescapes a hexadecimal HTML entity of ü" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#xFC;")) `shouldBe` "ü"

    -- A smiley is in unicode and does not fit into not ASCII nor latin1
    it "unescapes a utf8-encoded smiley 😀" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "😀")) `shouldBe` "😀"
    it "unescapes a decimal HTML entity of a smiley" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#128512;")) `shouldBe` "😀"
    it "unescapes a lower case hexadecimal HTML entity of a smiley" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#x1f600;")) `shouldBe` "😀"
    it "unescapes an upper case hexadecimal HTML entity of a smiley" $
      unHTMLText (LB.fromStrict (TE.encodeUtf8 "&#X1F600;")) `shouldBe` "😀"
