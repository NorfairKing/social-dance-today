{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://golatindance.com
--
-- This page has a large number of events in multiple cities
-- - 1. There are no terms of services.
-- - 2. There is no explicit copyright notice.
-- - 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable.
-- HOWER, there is a way to export events in ical format.
-- On each page, there is one, and there's one per category (city).
--
-- The Monthly calendar is available at
-- https://golatindance.com/events/category/:city/month/?ical=1
--
-- And this is a Tribe Calendar, see 'Salsa.Party.Importer.TribeCalendar'
--
-- The event calendar is available at
-- https://golatindance.com/event/:eventslug/:day
--
-- Unfortunately the page requires javascript to display any events.
--
-- It looks like event pages DO actually contain JSON LD, but they contain multiple of them so we can't just parse the first one.
--
-- We use the calendar exports to find the event pages to scrape from, then use JSON LD.
module Salsa.Party.Importer.GolatindanceCom (golatindanceComImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Importer.TribeCalendar
import Text.HTML.Scalpel

golatindanceComImporter :: Importer
golatindanceComImporter =
  Importer
    { importerName = "golatindance.com",
      importerFunc = func,
      importerUserAgent = UserAgentSocial,
      importerTimezoneOffset = -6 -- Texas timezone
    }

func :: Import ()
func = do
  runConduit $
    yield "https://golatindance.com/"
      .| C.concatMap (parseRequest :: String -> Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeBodyC scrapeCategoryUrls
      .| C.mapM shuffleList
      .| C.concat
      .| deduplicateC
      .| tribeCalendarC
      .| C.filter (isPrefixOf "https://golatindance.com/event/" . show)
      .| C.concatMap (requestFromURI :: URI -> Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| jsonLDEventsC
      .| tribeCalendarJSONLDEvents
      .| C.map
        ( first $ \ee -> case externalEventHomepage ee of
            Nothing -> ee {externalEventHomepage = Just $ externalEventOrigin ee}
            Just _ -> ee
        )
      .| C.mapM_ importExternalEventWithMImage_

scrapeCategoryUrls :: ScraperT Text Import [URI]
scrapeCategoryUrls = do
  refs <- attrs "href" "a"
  pure $ mapMaybe (parseURI . T.unpack) $ filter ("https://golatindance.com/events/category/" `T.isPrefixOf`) refs
