{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://salsachicago.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- There is actually a sitemap, but there is also a tribe calendar so that will
-- probably be easiest.
module Salsa.Party.Importer.SalsachicagoCom (salsachicagoComImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Importer.TribeCalendar

salsachicagoComImporter :: Importer
salsachicagoComImporter =
  Importer
    { importerName = "salsachicago.com",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = -6 -- Chicago time
    }

func :: Import ()
func = do
  runConduit $
    yield "http://salsachicago.com/events"
      .| C.concatMap (parseURI :: String -> Maybe URI)
      .| tribeCalendarC
      .| C.filter (isPrefixOf "http://salsachicago.com/event/" . show)
      .| C.concatMap (requestFromURI :: URI -> Maybe Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| jsonLDEventsC
      .| importTribeCalendarJSONLDEvents
