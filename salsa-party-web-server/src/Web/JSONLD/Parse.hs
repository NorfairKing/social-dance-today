{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.JSONLD.Parse where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Text.HTML.Scalpel

scrapeJSONLDValues :: Monad m => ScraperT Text m [JSON.Value]
scrapeJSONLDValues = do
  lbs <- scrapeJSONLDText
  pure $ mapMaybe (JSON.decode . LB.fromStrict . TE.encodeUtf8) lbs

scrapeJSONLDText :: Monad m => ScraperT Text m [Text]
scrapeJSONLDText = texts ("script" @: ["type" @= "application/ld+json"])
