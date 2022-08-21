{-# LANGUAGE OverloadedStrings #-}

-- | https://wannadance.com
--
-- * There is a robots.txt but it only disallows admin pages.
-- * There is a terms of service but it doesn't mention crawling or search engine indexing
--
-- Thoughts while coming up with a strategy.
-- * Each event page like https://wannadance.com/listing/international-salsa-festival-hamburg-2022/ has a relatively scrape-able page.
--   There is also linked data but it does not describe an event.
--   It describes a local business instead.
--   Maybe we can combine the two.
--  It's finding the events that will be difficult.
-- * There is a sitemap, but it is one that is generated by wordpress so it's a bit of a mess.
-- * There is a sitemap that lists pages per region:
--   https://wannadance.com/wp-sitemap-taxonomies-region-1.xml
-- * There is a sitemap that lists pages per dance style:
--   https://wannadance.com/wp-sitemap-taxonomies-job_listing_category-1.xml
-- * There is a sitemap that lists all sorts of things, among which are events:
--   https://wannadance.com/wp-sitemap-posts-job_listing-1.xml
-- * All these sitemaps are terribly named.
-- * The sitemaps are available in xml and in html as well it seems.
--
--
-- This is the overal strategy:
module Salsa.Party.Importer.WannaDanceCom (wannaDanceCom) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

wannaDanceCom :: Importer
wannaDanceCom =
  Importer
    { importerName = "wannadance.com",
      importerFunc = func
    }

func :: Import ()
func = pure ()
