{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- A calendar from https://theeventscalendar.com/
--
-- This is a wordpress plugin:
-- https://theeventscalendar.com/products/wordpress-events-calendar/
--
-- At a tribe calendar url, you can add
-- /list/?tribe-bar-date=YYYY-MM-DD&ical=1
-- to the end to get an ICS file
--
-- This ICS file can then contain a bunch of events and those have URLs, which we can try to crawl.
--
-- Developer docs here:
-- https://theeventscalendar.com/knowledgebase/k/new-user-primer-the-events-calendar-and-events-calendar-pro/
module Salsa.Party.Importer.TribeCalendar (tribeCalendarC) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import

-- For a given URL, find the tribe calendar and get all the event URLs in there.
tribeCalendarC :: ConduitT URI URI Import ()
tribeCalendarC =
  andDays
    .| C.concatMap makeCalendarRequest
    .| doHttpRequestWith
    .| logRequestErrors
    .| parseUrlsInCalendars
    .| deduplicateC

-- | Make (but don't send) a calendar request for the given calendar URI and day.
makeCalendarRequest :: (URI, Day) -> Maybe HTTP.Request
makeCalendarRequest (uri, day) = do
  requestPrototype <- requestFromURI $ uri {uriPath = uriPath uri <> "/list/"}
  pure $
    setQueryString
      [ ("tribe-bar-date", Just $ TE.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%F" day),
        ("ical", Just "1")
      ]
      $ requestPrototype
        { requestHeaders = ("Accept", "application/calendar") : requestHeaders requestPrototype
        }

-- | Parse the URLs in an ICS file
--
-- Instead of parsing the actual ICS file, because they probably aren't valid
-- (I tried), we just take the URLs that are on a line that says "URL: "
parseUrlsInCalendars :: ConduitT (HTTP.Request, Response LB.ByteString) URI Import ()
parseUrlsInCalendars =
  C.map (responseBody . snd)
    -- Unbounded is not safe here, but not sure what to do about it ..
    .| C.splitOnUnboundedE (== 0x0a)
    .| C.concatMap (LB.stripPrefix "URL:")
    .| C.map (\lb -> fromMaybe lb $ LB.stripSuffix "\r" lb) -- Strip \r if there is one.
    .| C.map LB.toStrict
    .| C.concatMap TE.decodeUtf8'
    .| C.concatMap (parseURI . T.unpack)
