{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://salsa.faurax.fr
--
-- * There are no terms of service
-- * The robots.txt allow everything except the /admin route
--
--
-- Our strategy is as follows:
--
-- 1. On the homepage there is a list of events.
-- 2. On each event page, we can scrape the pieces of the events.
module Salsa.Party.Importer.SalsaFauraxFr (salsaFauraxFrImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

salsaFauraxFrImporter :: Importer
salsaFauraxFrImporter =
  Importer
    { importerName = "salsa.faurax.fr",
      importerFunc = func,
      importerUserAgent = UserAgentRandom
    }

func :: Import ()
func =
  runConduit $
    yield "https://salsa.faurax.fr"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| C.concatMap
        ( \(request, response) ->
            (,) request <$> traverse TE.decodeUtf8' (LB.toStrict <$> response)
        )
      .| scrapeEventLinks
      .| C.concatMap (requestFromURI :: URI -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| C.concatMap
        ( \(request, response) ->
            (,) request <$> traverse TE.decodeUtf8' (LB.toStrict <$> response)
        )
      .| scrapeEventPage
      .| C.mapM_ importExternalEvent

scrapeEventLinks :: ConduitT (HTTP.Request, HTTP.Response Text) URI Import ()
scrapeEventLinks = awaitForever $ \(request, response) -> do
  let links = fromMaybe [] $
        scrapeStringLike (responseBody response) $
          chroots ("div" @: [hasClass "soiree", hasClass "vevent"]) $ do
            attr "href" ("a" @: [hasClass "url"])
  let uris = mapMaybe (\link -> parseURI $ show (getUri request) <> T.unpack link) links
  yieldMany uris -- Not shuffled, because they

eventUrlPrefix :: Text
eventUrlPrefix = "https://salsa.faurax.fr//index.php/evt/"

scrapeEventPage :: ConduitT (HTTP.Request, HTTP.Response Text) ExternalEvent Import ()
scrapeEventPage = awaitForever $ \(request, response) -> do
  events <- lift $
    fmap (fromMaybe []) $
      scrapeStringLikeT (responseBody response) $
        chroots ("div" @: [hasClass "soiree", hasClass "vevent"]) $ do
          externalEventUuid <- nextRandomUUID

          let uriText = T.pack (show (getUri request))
          let externalEventKey = fromMaybe uriText $ T.stripPrefix eventUrlPrefix uriText

          externalEventTitle <- text ("span" @: [hasClass "summary"])

          address <- text ("span" @: [hasClass "location"])
          app <- asks importEnvApp
          mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
          externalEventPlace <- case mPlaceEntity of
            Nothing -> fail "could not geolocate"
            Just (Entity placeId _) -> pure placeId

          externalEventDescription <- optional $ text ("p" @: [hasClass "comm"])

          -- There is no organiser on the page
          let externalEventOrganiser = Nothing

          dayText <- attr "title" ("abbr" @: [hasClass "dtstart"])
          LocalTime externalEventDay start <- case parseTimeM True defaultTimeLocale "%FT%H:%M:%SZ" (T.unpack dayText) of
            Nothing -> fail "unreadable day"
            Just d -> pure d

          let externalEventStart = Just start

          -- TODO There is a homepage on the page but it's hard to scrape
          let externalEventHomepage = Nothing
          -- TODO There is a price on the page but it's hard to scrape
          let externalEventPrice = Nothing

          -- There is no information about cancellation on the page.
          let externalEventCancelled = Nothing

          -- There are no posters on the page.
          let externalEventPoster = Nothing

          externalEventCreated <- liftIO getCurrentTime
          let externalEventModified = Nothing
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request

          let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
          pure ExternalEvent {..}
  yieldManyShuffled events
