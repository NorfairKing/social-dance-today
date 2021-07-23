{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.JSONLD.Parse where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Text.HTML.Scalpel

scrapeJSONLDValues :: Monad m => ScraperT LB.ByteString m [JSON.Value]
scrapeJSONLDValues = do
  lbs <- scrapeJSONLDText
  pure $ mapMaybe JSON.decode lbs

scrapeJSONLDText :: Monad m => ScraperT LB.ByteString m [LB.ByteString]
scrapeJSONLDText = texts ("script" @: ["type" @= "application/ld+json"])
