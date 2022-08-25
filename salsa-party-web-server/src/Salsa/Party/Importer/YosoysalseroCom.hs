{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://yosoysalsero.com
--
-- 1. There are terms of use that seem to prohibit reproduction, but they don't block search engines.
--    They also explicitly mention that crawlers should not impose an unreasonable burden.
-- 2. There is a robots.txt that does not prohibit anything
-- 3. There is a sitemap that has many links.
--
-- The front page shows a bunch of events, and they do indeed each have an event page like this:
-- https://agenda.yosoysalsero.com/parties/6218d4108ff33c001d2e0d32
-- However, the links are loaded dynamically so we can't scrape them.
-- Luckily we see in the network tab of the browser that they contact an api here:
-- https://strapi.yosoysalsero.com/events/feed
--
-- A quick local curl shows us that we can just contact it ourselves.
module Salsa.Party.Importer.YosoysalseroCom (yosoysalseroComImporter) where

import Conduit
import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.Read (readMaybe)

yosoysalseroComImporter :: Importer
yosoysalseroComImporter =
  Importer
    { importerName = "yosoysalsero.com",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $ pure ()
