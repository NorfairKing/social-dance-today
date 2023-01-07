{-# LANGUAGE OverloadedStrings #-}

-- | https://stayhappening.com
--
-- * There is no robots.txt
-- * There is a terms of service but it doesn't mention crawling or search engine indexing
-- * There are jsonld events
--
-- This is the overal strategy:
--
-- 1. There are links to locations on the front page, we find those first.
-- 2. On each of those pages, there are all events, but we don't want all events.
--    We only want the salsa/bachata/kizomba/zouk/tango events.
--    None of those except salsa actually have any sort of filter.
--    However, if we add "--salsa" to the end, we get the salsa parties only.
--    There is no link to this, so I don't know how we know about this, but here we are.
--    So we turn each link like
--    https://stayhappening.com/london
--    into
--    https://stayhappening.com/london--salsa
--
-- 3. On each of the location pages filtered by salsa, we find event links that look like this:
--    https://stayhappening.com/e/cuban-all-stars-presents-buena-vista-E2ISUJQ1AG7
--    Note the /e/ in the url.
--    We parse each of those.
--
-- 4. On each of the event pages, there is a JSON.LD event, so we can import those as is.
--
-- NOTE: Sometimes we get a 403 response, but it is not clear when.
-- It has happened with these user agents:
--
-- * "Mozilla/4.0 (compatible; MSIE 6.0; MSIE 5.5; Windows NT 5.0) Opera 7.02 Bork-edition [en]"
module Salsa.Party.Importer.StayHappeningCom (stayHappeningComImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel

stayHappeningComImporter :: Importer
stayHappeningComImporter =
  Importer
    { importerName = "stayhappening.com",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = -5 -- Eastern standard time
    }

func :: Import ()
func =
  runConduit $
    yield "https://stayhappening.com/"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeBodyC scrapeLocationLinks
      .| C.mapM shuffleList
      .| C.concat
      .| deduplicateC
      .| C.concatMap makeLocationSalsaEventsRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeBodyC scrapeEventLinks
      .| C.mapM shuffleList
      .| C.concat
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| jsonLDEventsC
      .| convertLDEventToExternalEvent eventUrlPrefix
      .| C.mapM_ importExternalEventWithMImage_

scrapeLocationLinks :: ScraperT Text Import [Text]
scrapeLocationLinks = do
  refs <- attrs "href" "a"
  pure $ filter ("https://stayhappening.com/" `T.isPrefixOf`) refs

makeLocationSalsaEventsRequest :: Text -> Maybe Request
makeLocationSalsaEventsRequest t = parseRequest $ T.unpack t <> "--salsa"

scrapeEventLinks :: ScraperT Text Import [Text]
scrapeEventLinks = do
  refs <- attrs "href" "a"
  pure $ filter (eventUrlPrefix `T.isPrefixOf`) refs

makeEventRequest :: Text -> Maybe Request
makeEventRequest = parseRequest . T.unpack

eventUrlPrefix :: Text
eventUrlPrefix = "https://stayhappening.com/e/"
