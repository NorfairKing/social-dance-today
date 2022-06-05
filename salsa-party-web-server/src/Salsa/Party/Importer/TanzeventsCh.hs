{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://tanzevents.ch
--
-- 1. There is a robots.txt but it does not disallow any crawling.
-- 2. There is no Sitemap
-- 3. There are terms of service but they don't mention crawling.
--
-- The strategy is as follows:
-- 1. There are dance styles here:
--    https://tanzevents.ch/-tanzen/de/events/
--    We scrape the links and fetch each of these pages.
-- 2. Each of these pages contain a list of events.
--    There is no page per event, but there are good class names that we can crawl.
module Salsa.Party.Importer.TanzeventsCh (tanzeventsChImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

tanzeventsChImporter :: Importer
tanzeventsChImporter =
  Importer
    { importerName = "tanzevents.ch",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    yield "https://tanzevents.ch/-tanzen/de/events/"
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeStyleLinks
      .| C.concatMap makeLocationStyleRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseEventsFromStylePage
      .| C.mapM_ importExternalEvent

scrapeStyleLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) URI m ()
scrapeStyleLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $
          chroot ("div" @: [hasClass "dance_groups"]) $ do
            refs <- attrs "href" "a"
            pure $
              map (`relativeTo` getUri request) $
                mapMaybe (parseURIReference . T.unpack) $
                  mapMaybe maybeUtf8 refs
  yieldManyShuffled uris

makeLocationStyleRequest :: URI -> Maybe Request
makeLocationStyleRequest = requestFromURI

parseEventsFromStylePage :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) ExternalEvent Import ()
parseEventsFromStylePage = awaitForever $ \(request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let yesterday = addDays (-1) today
  let scrapeExternalEventsFromPage :: ScraperT LB.ByteString Import [ExternalEvent]
      scrapeExternalEventsFromPage =
        chroot ("div" @: [hasClass "dynpg_AA_row_Table"]) $
          chroots ("div" @: [hasClass "row"]) $ do
            rawTitle <- text $ "div" @: [hasClass "dynpg_e_header_text"]
            let externalEventTitle = TE.decodeUtf8With TE.ignore (LB.toStrict rawTitle)

            let externalEventDescription = Nothing :: Maybe Text
            (externalEventDay, externalEventStart) <- chroot ("div" @: [hasClass "when"]) $ do
              day <- chroot ("div" @: [hasClass "date"]) $ do
                rawDay <- text $ "span" @: [hasClass "day"]
                dayText <- utf8 rawDay
                rawMonth <- text $ "span" @: [hasClass "month"]
                monthText <- utf8 rawMonth
                rawYear <- text $ "span" @: [hasClass "year"]
                yearText <- utf8 rawYear
                let dateText = T.concat [yearText, " ", monthText, ". ", dayText]

                day <- case parseTimeM True germanTimeLocale "%Y %b %d." (T.unpack dateText) of
                  Nothing -> fail "day not parseable"
                  Just d -> pure (d :: Day)
                guard (day >= yesterday)

                pure day

              hour <- optional $ do
                rawHour <- text $ "div" @: [hasClass "hour"]
                hourText <- utf8 rawHour
                case parseTimeM True germanTimeLocale "%H:%M Uhr" (T.unpack hourText) of
                  Nothing -> fail "hour not parseable"
                  Just h -> pure (h :: TimeOfDay)

              pure (day, hour)

            let externalEventPrice = Nothing -- There is a price, but it's hard to scrape
            let externalEventCancelled = Nothing

            externalEventHomepage <- optional $
              chroot ("p" @: [hasClass "event_url"]) $ do
                ref <- attr "href" "a"
                let link = TE.decodeUtf8With TE.ignore (LB.toStrict ref)
                pure link

            forM_ externalEventHomepage $ \link -> do
              let isLocal = T.isPrefixOf "https://social-dance.today/" link
              guard $ not isLocal

            let externalEventOrganiser = Nothing

            let externalEventCreated = now
            let externalEventModified = Nothing
            externalEventImporter <- asks importEnvId
            let externalEventOrigin = T.pack $ show $ getUri request

            externalEventPlace <- do
              rawAddress <- text ("span" @: [hasClass "shorttext"])
              let addressText = TE.decodeUtf8With TE.ignore (LB.toStrict rawAddress)
              let address = T.unwords $ filter (not . T.null) $ map T.strip $ T.words addressText

              app <- asks importEnvApp
              mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
              case mPlaceEntity of
                Nothing -> fail "could not geolocate"
                Just (Entity placeId _) -> pure placeId

            -- Theres' no natural key, so we just make one up
            let externalEventKey = T.pack $ show (getUri request) <> formatTime defaultTimeLocale "%F" externalEventDay <> T.unpack externalEventTitle
            externalEventUuid <- nextRandomUUID
            let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

            pure ExternalEvent {..}

  externalEvents <- lift $ fmap (fromMaybe []) $ scrapeStringLikeT (responseBody response) scrapeExternalEventsFromPage
  yieldMany (externalEvents :: [ExternalEvent])
