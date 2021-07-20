{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

--- 1. There are no terms of services.
--- 2. There is no explicit copyright notice.
--- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable.
-- HOWER, there is a way to export events in ical format.
-- On each page, there is one, and there's one per category (city).
--
-- The Monthly calendar is available at
-- https://golatindance.com/events/category/:city/month/?ical=1
--
-- There is also a monthly calendar available for months other than the current month at
-- https://golatindance.com/events/category/:city/YYYY-MM/?ical=1
--
-- There is also a list-based calendar available for what seems an indeterminate amount of time forward
-- using;
-- https://golatindance.com/events/category/:city/list/?tribe-bar-date=YYYY-MM-DD&ical=1
--
-- Events are also paginated, so when listing events you can do this too:
-- https://golatindance.com/events/category/london/page/2/
--
-- The event calendar is available at
-- https://golatindance.com/event/:eventslug/:day
--
-- Unfortunately the page requires javascript to display any events.
--
-- It looks like event pages DO actually contain JSON LD, but they contain multiple of them so we can't just parse the first one.
--
-- First idea: Import events for the coming months directly from the monthly calendar exports.
--
-- Unfortunately the .ics files that the page generates don't actually parse.
--
-- HOWEVER: they do still contain lines like this:
-- https://golatindance.com/event/todo-latino-tuesdays-with-latin-collective-urban-kiz-uk/2021-08-03/
-- so we can actually ignore the fact that it's ICS and go look for those lines instead.
--
-- Second idea: Use the calendar exports to find the event pages to scrape from, then use JSON LD.
--

-- | https://golatindance.com
--
-- This page has a large number of events in multiple cities
module Salsa.Party.Importer.GolatindanceCom (golatindanceComImporter) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Conduit.Combinators as C
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.ICalendar.Parser as ICal

golatindanceComImporter :: Importer
golatindanceComImporter =
  Importer
    { importerName = "danceus.org",
      importerFunc = func
    }

logname = "Importer-golatindance.com"

func :: Import ()
func = do
  runConduit $
    yieldMany categories
      .| C.concatMap makeCalendarRequest
      .| C.mapM doHttpRequest
      .| logRequestErrors
      .| parseUrlsInCalendars
      .| C.mapM_ (liftIO . print)

makeCalendarRequest :: Text -> Maybe HTTP.Request
makeCalendarRequest city = do
  let baseUrl = "https://golatindance.com"
      categoryIcalUrl category = baseUrl <> "/events/category/" <> category <> "/list"
  requestPrototype <- parseRequest $ categoryIcalUrl "london"
  pure $ setQueryString [("tribe-bar-date", Just "2021-07-21"), ("ical", Just "1")] $ requestPrototype {requestHeaders = ("Accept", "application/calendar") : requestHeaders requestPrototype}

logRequestErrors :: ConduitT (Either HttpException (Response LB.ByteString)) (Response LB.ByteString) Import ()
logRequestErrors = awaitForever $ \case
  Left err -> logErrorNS logname $ T.pack $ unlines ["Error while fetching calendar page: " <> ppShow err]
  Right response -> yield response

parseUrlsInCalendars :: ConduitT (Response LB.ByteString) Text Import ()
parseUrlsInCalendars =
  C.map responseBody
    -- Unbounded is not safe here, but not sure what to do about it ..
    .| C.splitOnUnboundedE (== 0x0a)
    .| C.concatMap (LB.stripPrefix "URL:")
    .| C.map LB.toStrict
    .| C.concatMap TE.decodeUtf8'

categories :: [Text]
categories =
  [ "london"
  ]

-- [ "melbourne",
--   "sydney",
--   "toronto",
--   "vancouver"
-- ]

-- TODO the rest of the categories under 'Event Calendars' here: https://golatindance.com/
