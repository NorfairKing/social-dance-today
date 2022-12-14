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
-- 3. On each of those pages, there's a "See More Events" button, we fetch that page.
-- 4. On each of the search result pages, there are search results with links to events, as well as potentially more result pages.
--    We fetch each of those result pages.
module Salsa.Party.Importer.DanceusOrg (danceusOrgImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel
import qualified Web.JSONLD as LD

danceusOrgImporter :: Importer
danceusOrgImporter =
  Importer
    { importerName = "danceus.org",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = -5 -- Eastern Standard Time
    }

func :: Import ()
func =
  runConduit $
    yieldManyShuffled ("https://www.danceus.org/events/" : ["https://www.danceus.org/events/" <> dance <> "/" | dance <- dances])
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeLocationLinks
      .| deduplicateC
      .| C.concatMap makeLocationRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeSeeMoreEventsLinks
      .| C.concatMap makeSeeMoreEventsRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| alsoScrapeOtherSearchResultPages
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| jsonLDEventsC
      .| scrapImageIfStatic
      .| convertLDEventToExternalEvent eventUrlPrefix
      .| C.mapM_ importExternalEventWithMImage_

dances :: [String]
dances =
  [ "argentine-tango",
    "salsa",
    "swing"
  ]

scrapeLocationLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response Text) URI m ()
scrapeLocationLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter (\l -> "/events/" `T.isPrefixOf` l && "-calendar/" `T.isSuffixOf` l) refs
  yieldManyShuffled $ map (`relativeTo` getUri request) $ mapMaybe (parseURIReference . T.unpack) uris

makeLocationRequest :: URI -> Maybe Request
makeLocationRequest = requestFromURI

scrapeSeeMoreEventsLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response Text) URI m ()
scrapeSeeMoreEventsLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $
          chroot ("div" @: [hasClass "see-more-button-container"]) $ do
            refs <- attrs "href" ("a" @: [hasClass "see-more-button"])
            pure $ filter ("/search-events/" `T.isPrefixOf`) refs
  yieldMany $ map (`relativeTo` getUri request) $ mapMaybe (parseURIReference . escapeURIString isUnescapedInURI . T.unpack) uris

makeSeeMoreEventsRequest :: URI -> Maybe Request
makeSeeMoreEventsRequest = requestFromURI

alsoScrapeOtherSearchResultPages :: ConduitT (HTTP.Request, HTTP.Response Text) (HTTP.Request, HTTP.Response Text) Import ()
alsoScrapeOtherSearchResultPages = do
  awaitForever $ \(request, response) -> do
    yield (request, response)
    go (request, response)
  where
    go :: (HTTP.Request, HTTP.Response Text) -> ConduitT (HTTP.Request, HTTP.Response Text) (HTTP.Request, HTTP.Response Text) Import ()
    go (request, response) =
      if HTTP.statusCode (HTTP.responseStatus response) == 404
        then pure () -- End reached
        else do
          let mLink :: Maybe Text
              mLink =
                scrapeStringLike (responseBody response) $ do
                  chroot ("ul" @: [hasClass "pagination"]) $
                    chroot ("li" @: [notP $ hasClass "disabled"]) $ do
                      ref <- attr "href" ("a" @: ["aria-label" @= "Next"])
                      guard $ "/search-events/" `T.isPrefixOf` ref
                      pure ref
          case mLink >>= fmap (`relativeTo` getUri request) . parseURIReference . escapeURIString isUnescapedInURI . T.unpack >>= requestFromURI of
            Nothing -> logDebugN $ T.pack $ unwords ["Pages end here:", show (getUri request)]
            Just request' -> do
              logDebugN $ T.pack $ unwords ["Trying to fetch the next page", show (getUri request')]
              errOrResponse' <- lift $ left show <$> doHttpRequest request'
              case errOrResponse' >>= (left show . parseHttpBodyText) of
                Left err -> logErrorN $ T.pack $ unlines ["Error while fetching page: " <> ppShow err]
                Right response' -> do
                  yield (request', response')
                  go (request', response')

scrapeEventLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response Text) URI m ()
scrapeEventLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter ("/event/" `T.isPrefixOf`) refs
  yieldManyShuffled $ map (`relativeTo` getUri request) $ mapMaybe (parseURIReference . T.unpack) uris

scrapImageIfStatic :: ConduitT (LD.Event, (Request, Response Text)) (LD.Event, (Request, Response Text)) Import ()
scrapImageIfStatic = awaitForever $ \(ldEvent, (request, response)) -> do
  let mIsStaticImage =
        scrapeStringLike (responseBody response) $
          chroot "section" $
            chroot ("div" @: [hasClass "cover-image-container", hasClass "in-content-cover"]) $ do
              attr "src" "img"
  letThrough <- case mIsStaticImage of
    Nothing -> do
      logWarnN "Could not figure out if the image was static or not."
      pure True
    Just src ->
      if src == "/static/img/event-cover.jpg"
        then do
          logDebugN "Static image, not fetching it."
          pure False
        else do
          logDebugN "Non-static image, fetching it."
          pure True
  let modifiedEvent =
        if letThrough
          then ldEvent
          else ldEvent {LD.eventImages = []}
  yield (modifiedEvent, (request, response))

makeEventRequest :: URI -> Maybe Request
makeEventRequest = requestFromURI

eventUrlPrefix :: Text
eventUrlPrefix = "https://www.danceus.org/event/"
