{-# LANGUAGE OverloadedStrings #-}

-- | https://danceus.org
--
-- * There is no sitemap.xml
-- * There's a terms of service here: https://www.danceus.org/about/terms/
--   They mention:
--   "you may not reproduce, modify, distribute or republish materials
--   contained on this site (either directly or by linking)"
--   but I'm pretty sure they cannot say that when they leave the robots.txt
--   open.
--   Let's still be respectful about our crawling.
-- * There is a robots.txt and it disallows some things, but none of the things they link to, it seems.
--
-- Our strategy is as follows:
--
-- 1. There is a list of calendars here: https://www.danceus.org/events/
--    We scrape all the links that end in "-calendar/" and leave all the ones
--    that end in "-classes/".
--
-- 2. We fetch each of these links.
--
-- 3. There are JSONLD events right on these pages, so we can import those as is.
module Salsa.Party.Importer.DanceusOrg (danceusOrgImporter) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

danceusOrgImporter :: Importer
danceusOrgImporter =
  Importer
    { importerName = "danceus.org",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    yield "https://www.danceus.org/events/"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeLocationLinks
      .| deduplicateC
      .| C.concatMap makeLocationRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| jsonLDEventsC
      .| convertLDEventToExternalEvent eventUrlPrefix
      .| C.mapM_ importExternalEventWithMImage

scrapeLocationLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) URI m ()
scrapeLocationLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter (\l -> "/events/" `T.isPrefixOf` l && "-calendar/" `T.isSuffixOf` l) $ mapMaybe maybeUtf8 refs
  yieldManyShuffled $ map (`relativeTo` getUri request) $ mapMaybe (parseURIReference . T.unpack) uris

makeLocationRequest :: URI -> Maybe Request
makeLocationRequest = requestFromURI

scrapeEventLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) URI m ()
scrapeEventLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter ("/event/" `T.isPrefixOf`) $ mapMaybe maybeUtf8 refs
  yieldManyShuffled $ map (`relativeTo` getUri request) $ mapMaybe (parseURIReference . T.unpack) uris

makeEventRequest :: URI -> Maybe Request
makeEventRequest = requestFromURI

eventUrlPrefix :: Text
eventUrlPrefix = "https://www.danceus.org/event/"
