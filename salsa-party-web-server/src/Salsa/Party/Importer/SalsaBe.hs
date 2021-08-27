{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://salsa.be
--
-- 1. There are no terms of services.
-- 2. There is an explicit copyright notice at http://www.salsa.be/disclaimer.htm but it seems to allow crawling for search engines.
-- 3. There is no robots.txt
--
-- All good so far, except the data is not machine readable and there is no sitemap.xml.
--
-- However shit this website may be for userso, it's pretty GREAT for me right now.
-- There is a search page at http://www.salsa.be/vcalendar/search.php, and we can search programmatically with just a get request and query parameters like so:
-- http://www.salsa.be/vcalendar/search.php?s_keyword=&s_category=&s_event_date_from=1%2F8%2F2021&s_event_date_to=31%2F8%2F2022
--
-- The response then contains a table of events, with one link each.
-- If we follow those links, we get to pages like this:
-- http://www.salsa.be/vcalendar/event_view.php?event_id=274240
--
-- They're not exactly machine readible per-se, but at least they're fast, always the same format, and HTML-only!
module Salsa.Party.Importer.SalsaBe (salsaBeImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import Text.Read (readMaybe)

salsaBeImporter :: Importer
salsaBeImporter =
  Importer
    { importerName = "salsa.be",
      importerFunc = func
    }

func :: Import ()
func = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let yesterday = addDays (-1) today
      endDay = addDays daysToImportAhead today
  requestPrototype <- liftIO $ parseRequest "http://www.salsa.be/vcalendar/search.php"
  let formatDay = TE.encodeUtf8 . T.pack . formatTime defaultTimeLocale "%e/%m/%Y"
  let queryParams = [("s_keyword", Nothing), ("s_category", Nothing), ("s_event_date_from", Just $ formatDay yesterday), ("s_event_date_to", Just $ formatDay endDay)]
  let request = setQueryString queryParams requestPrototype
  runConduit $
    yield request
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| importEventPage

scrapeEventLinks :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Text Import ()
scrapeEventLinks = awaitForever $ \(request, response) -> do
  let mrefs = scrapeStringLike (responseBody response) $ do
        refss <- chroots "td" $ attrs "href" "a"
        pure $ mapMaybe maybeUtf8 $ concat refss
  forM_ mrefs $ \refs -> do
    let eventIds = mapMaybe (T.stripPrefix "event_view.php?event_id=") refs
    yieldMany eventIds

makeEventRequest :: Text -> Maybe HTTP.Request
makeEventRequest eventId = parseRequest $ "http://www.salsa.be/vcalendar/event_view.php?event_id=" <> T.unpack eventId

importEventPage :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Void Import ()
importEventPage = awaitForever $ \(request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let eventScraper :: ScraperT LB.ByteString Import ExternalEvent
      eventScraper = do
        pure ExternalEvent {..}
  lift $ do
    mExternalEvent <- scrapeStringLikeT (responseBody response) eventScraper
    mapM importExternalEvent mExternalEvent
