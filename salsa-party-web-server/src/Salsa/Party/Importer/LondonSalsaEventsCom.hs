{-# LANGUAGE OverloadedStrings #-}

-- | https://londonsalsaevents.com
--
-- * There is a robots.txt that does not disallow any crawling
-- * There is no terms of service
-- * There are jsonld events
--
-- This is the overal strategy:
--
-- 1. There are two relevant pages, the parties and the afternoon socials.
--    We fetch those lists.
-- 2. On each of the respective pages, there are links to event pages of this form:
--    https://londonsalsaevents.com/event-pro/2423/
--    We fetch those pages
-- 3  On each of those pages, there's an LD event, we import those.
module Salsa.Party.Importer.LondonSalsaEventsCom (londonSalsaEventsComImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel

londonSalsaEventsComImporter :: Importer
londonSalsaEventsComImporter =
  Importer
    { importerName = "londonsalsaevents.com",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = 0 -- London time
    }

func :: Import ()
func =
  runConduit $
    yieldManyShuffled ["https://londonsalsaevents.com/list-events/", "https://londonsalsaevents.com/afternoon-social/"]
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| jsonLDEventsC
      .| convertLDEventToExternalEvent eventUrlPrefix
      .| C.mapM_ importExternalEventWithMImage_

scrapeEventLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response Text) Text m ()
scrapeEventLinks = awaitForever $ \(_, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter (eventUrlPrefix `T.isPrefixOf`) refs
  yieldManyShuffled uris

eventUrlPrefix :: Text
eventUrlPrefix = "https://londonsalsaevents.com/event-pro/"

makeEventRequest :: Text -> Maybe Request
makeEventRequest = parseRequest . T.unpack
