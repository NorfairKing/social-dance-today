{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://danceplace.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable.
--
-- The robots.txt contains a line that mentions a sitemap which is a text file.
--
-- > https://www.danceplace.com/robots.txt:
--
-- > User-Agent: *
-- > Allow: /
-- >
-- > Sitemap: https://www.danceplace.com/assets/site-map-31-07-20.txt
--
-- Not sure whether that's allowed but that text file contains all links to all events.
--
-- The text file contains an index of everything on the site, including events.
-- * Urls like this:
--   https://www.danceplace.com/index/no/12/Flying+Dog-Waterloo_+ON-Canada-Place+to+social+dance+Salsa
--   Which are weird, events from 60 years ago?!
--
-- * Urls that end in +school, which look like schools.
--   https://www.danceplace.com/index/no/626/Air+De+Tango+-Montreal_+QC-Canada-Tango+Dance+school
--
-- * Urls that end in +event, which look like events:
--   https://www.danceplace.com/index/no/8177/BachataStars+Poland-2020-Warsaw-Poland-Bachata+Dance+event
--   The event urls also always seem to contain the year that they're in.
--
-- The event pages are not machine readible, but there are some nice benefits anyway.
--
-- * the title has a <span itemprop="name">
-- * the description has a <meta itemprop="description">
-- * The start as <meta itemprop="startDate" content="2021-03-05T00:00">
-- * The end date as <meta itemprop="endDate" content="2021-03-07T00:00">
module Salsa.Party.Importer.DanceplaceCom (danceplaceComImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

danceplaceComImporter :: Importer
danceplaceComImporter =
  Importer
    { importerName = "danceplace.com",
      importerFunc = func
    }

baseUrl :: String
baseUrl = "https://danceplace.com"

func :: Import ()
func = do
  now <- liftIO getCurrentTime
  let (currentYear, _, _) = toGregorian $ utctDay now
  case parseRequest $ baseUrl <> "/robots.txt" of
    Nothing -> logErrorN "Robots.txt url was invalid."
    Just request -> do
      errOrResponse <- doHttpRequest request
      case errOrResponse of
        Left err -> logErrorN $ T.pack $ "Could not reach robots.txt:\n" <> ppShow err
        Right response -> do
          let sitemapUrls = mapMaybe (SB.stripPrefix "Sitemap: ") $ SB8.lines $ LB.toStrict $ responseBody response
          runConduit $
            yieldMany sitemapUrls
              .| C.concatMap (parseRequest . SB8.unpack :: ByteString -> Maybe Request)
              .| doHttpRequestWith
              .| logRequestErrors
              .| C.map (responseBody . snd)
              .| C.splitOnUnboundedE (== 0x0a)
              .| C.filter ("+event" `LB8.isSuffixOf`)
              .| C.map LB.toStrict
              .| C.filter
                ( \url ->
                    SB8.pack (show currentYear) `SB8.isInfixOf` url
                      || SB8.pack (show (succ currentYear)) `SB8.isInfixOf` url
                )
              .| deduplicateC
              .| C.concatMap (parseRequest . SB8.unpack :: ByteString -> Maybe Request)
              .| doHttpRequestWith
              .| logRequestErrors
              .| C.mapM_ (uncurry parseEventFromPage)

parseEventFromPage :: HTTP.Request -> HTTP.Response LB.ByteString -> Import ()
parseEventFromPage request response = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let scraper = do
        externalEventDay <- do
          rawDate <- attr "content" ("meta" @: ["itemprop" @= "startDate"])
          case maybeUtf8 rawDate >>= (parseTimeM True defaultTimeLocale "%FT%H:%M" . T.unpack) of
            Nothing -> fail "couldn't parse the day"
            Just d -> pure d

        guard $ externalEventDay >= addDays (-1) today

        let externalEventKey =
              let uriText = T.pack $ show $ getUri request
               in case T.stripPrefix "https://www.danceplace.com/index/no/" uriText of
                    Nothing -> uriText
                    Just suffix -> suffix

        externalEventTitle <- text "title" >>= utf8

        externalEventDescription <- mutf8 $ optional $ attr "content" ("meta" @: ["name" @= "description"])

        -- SOMETIMES the organiser is on the page, but it's probably not worth scraping.
        let externalEventOrganiser = Nothing

        -- There are starting times on the pages but they're most often wrong; 00:00
        let externalEventStart = Nothing

        externalEventHomepage <- mutf8 $ optional $ attr "href" ("a" @: ["itemprop" @= "url"])

        let externalEventPrice = Nothing

        -- We can't accurately parse the cancelled state because the pages list PostPoned even when the events are not.
        let externalEventCancelled = False

        let externalEventCreated = now
        let externalEventModified = Nothing
        externalEventImporter <- Just <$> asks importEnvId
        let externalEventOrigin = T.pack $ show $ getUri request
        externalEventUuid <- nextRandomUUID

        mImageUri <- mutf8 $ optional $ attr "content" $ "meta" @: ["itemprop" @= "image"]

        externalEventPlace <- do
          rawAddressPieces <- chroot ("span" @: ["itemprop" @= "address"]) $ texts ("span" @: [hasClass "text-danger"])
          rawAddress <- T.intercalate ", " . map T.strip <$> mapM utf8 rawAddressPieces
          let address = T.replace " , " ", " $ T.strip rawAddress
          app <- asks importEnvApp
          mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
          case mPlaceEntity of
            Nothing -> fail $ "Place not found: " <> show address
            Just (Entity placeId _) -> pure placeId

        pure (ExternalEvent {..}, mImageUri)
  mExternalEvent <- scrapeStringLikeT (responseBody response) scraper
  case mExternalEvent of
    Nothing -> pure ()
    Just (externalEvent, mImageUriText) -> importExternalEventAnd externalEvent $ \externalEventId -> forM_ (mImageUriText >>= parseURI . T.unpack) $ \uri -> do
      mImageId <- tryToImportImage uri
      forM_ mImageId $ \imageId -> do
        importDB $
          upsertBy
            (UniqueExternalEventPoster externalEventId)
            ( ExternalEventPoster
                { externalEventPosterExternalEvent = externalEventId,
                  externalEventPosterImage = imageId,
                  externalEventPosterCreated = now,
                  externalEventPosterModified = Nothing
                }
            )
            [ ExternalEventPosterImage =. imageId,
              ExternalEventPosterModified =. Just now
            ]
