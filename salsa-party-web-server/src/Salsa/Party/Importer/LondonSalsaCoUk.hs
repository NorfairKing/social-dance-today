{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://londonsalsa.co.uk
--
-- * There is no robots.txt.
-- * There is no terms of service.
--
-- Details:
--
-- This is a difficult one:
--
-- * There is no sitemap
-- * The event pages are not machine readable at all.
-- * There is an events search directory but it is hard to navigate
-- * The next page is retrieved using a POST request (?!) and I can't figure out how, so for now we'll only get the first page.
-- * The event pages are not at all machine-readable
--
-- Here is an example event page:
-- https://londonsalsa.co.uk/Event/322728
--
--
-- This is the overal strategy:
--
-- * Fetch this: https://londonsalsa.co.uk/SearchClasses?hdnTaskTab=Events&hdnTask=&postcode=&dist=0&venue=&day=0&SortBy=0
--   This gets us a event listing. We'll get all the event links from there.
module Salsa.Party.Importer.LondonSalsaCoUk (londonSalsaCoUkImporter) where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel
import Text.Read (readMaybe)

londonSalsaCoUkImporter :: Importer
londonSalsaCoUkImporter =
  Importer
    { importerName = "londonsalsa.co.uk",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = 0 -- London time
    }

func :: Import ()
func =
  runConduit $
    yieldMany ["Club night", "Party", "Congress"]
      .| C.map ("https://londonsalsa.co.uk/SearchClasses?hdnTaskTab=Events&hdnTask=&postcode=&dist=0&venue=&day=0&SortBy=0&EType=" <>)
      .| C.concatMap (parseRequest :: String -> Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeBodyC listingPageEventIdScraper
      .| C.concat
      .| deduplicateC
      .| C.map (eventUrlPrefix <>)
      .| C.map T.unpack
      .| C.concatMap (parseRequest :: String -> Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| importEventPage

eventUrlPrefix :: Text
eventUrlPrefix = "https://londonsalsa.co.uk/Event/"

listingPageEventIdScraper :: ScraperT Text Import [Text]
listingPageEventIdScraper = chroot ("table" @: ["id" @= "MainContent_tbllist"]) $ do
  refs <- attrs "href" "a"
  pure $ mapMaybe (T.stripPrefix "/Event/") refs

importEventPage :: ConduitT (HTTP.Request, HTTP.Response Text) Void Import ()
importEventPage = awaitForever $ \(request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let eventScraper :: ScraperT Text Import (ExternalEvent, Maybe URI)
      eventScraper = do
        externalEventUuid <- nextRandomUUID

        externalEventKey <- case T.stripPrefix eventUrlPrefix (T.pack (show (getUri request))) of
          Nothing -> fail "Failed to parse event key"
          Just k -> pure k

        externalEventTitle <- text ("span" @: ["id" @= "MainContent_EventListMain_ename_0"])

        externalEventPlace <- do
          addrLine1 <- attr "value" ("input" @: ["id" @= "MainContent_EventList_vadd1_0"])
          addrLine2 <- attr "value" ("input" @: ["id" @= "MainContent_EventList_vadd2_0"])
          city <- attr "value" ("input" @: ["id" @= "MainContent_EventList_vcity_0"])
          postcode <- attr "value" ("input" @: ["id" @= "MainContent_EventList_vpc_0"])
          let address = T.concat [addrLine1, " ", addrLine2, ", ", postcode, ", ", city]
          mLatLon <- fmap join $
            optional $ do
              latText <- attr "value" ("input" @: ["id" @= "MainContent_EventListMain_vlat_0"])
              lonText <- attr "value" ("input" @: ["id" @= "MainContent_EventListMain_vlon_0"])
              pure $ (,) <$> readMaybe (T.unpack latText) <*> readMaybe (T.unpack lonText)
          case mLatLon of
            Nothing -> geoLocateScraper address
            Just (lat, lon) ->
              lift $
                fmap entityKey $
                  importDB $
                    upsertBy
                      (UniquePlaceQuery address)
                      (Place {placeLat = lat, placeLon = lon, placeQuery = address})
                      []

        externalEventDay <- do
          rawDateText <- text ("span" @: ["id" @= "MainContent_EventListMain_lbledate_0"])
          -- Looks like this:
          -- 5 February 2023, 6 PM - 5 February 2023, 11 PM
          startDateTimeText <- case T.splitOn " - " rawDateText of
            [startDateTimeText, _] -> pure startDateTimeText
            _ -> fail "could not split date text."
          startDateText <- case T.splitOn ", " startDateTimeText of
            [startDateText, _] -> pure startDateText
            _ -> fail "could not split start date text."

          case parseTimeM True defaultTimeLocale "%e %B %Y" (T.unpack startDateText) of
            Nothing -> fail "failed to parse day"
            Just d -> do
              guard (d >= addDays (-1) today)
              pure d
        let externalEventStart = Nothing -- On the page but hard to parse
        let externalEventPrice = Nothing -- On the page but hard to parse
        let externalEventCancelled = Nothing -- Not on the page?
        externalEventOrganiser <-
          optional $
            text ("a" @: ["id" @= "MainContent_EventListMain_tname_0"])
        let externalEventHomepage = Nothing -- Not on the page
        externalEventDescription <- optional $ text ("span" @: ["id" @= "MainContent_EventList_lblinfo_0"])

        let externalEventPoster = Nothing
        let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
        let externalEventCreated = now
        let externalEventModified = Nothing
        externalEventImporter <- asks importEnvId
        let externalEventOrigin = T.pack $ show $ getUri request

        mImageUri <- fmap join $
          optional $ do
            refs <- attrs "src" ("input" @: ["id" @= "MainContent_EventList_imgevent_0"])
            pure $ find ("https://images.salus-joy.com/UploadedImage/" `T.isPrefixOf`) refs >>= parseURI . T.unpack

        pure (ExternalEvent {..}, mImageUri)

  let prospectScraper :: Entity ExternalEvent -> ScraperT Text Import Prospect
      prospectScraper (Entity externalEventId ExternalEvent {..}) = do
        prospectName <- text ("a" @: ["id" @= "MainContent_EventList_lblpromoter_0"])
        prospectEmailAddress <- text ("a" @: ["id" @= "MainContent_EventList_temail_0"])

        -- The organiser and the event are on the same page and there is only one address
        let prospectPlace = Just externalEventPlace

        let prospectExternalEvent = Just externalEventId

        let prospectCreated = now
        let prospectModified = Nothing
        prospectSecret <- nextRandomUUID
        let prospectUnsubscribed = Nothing
        let prospectInvited = Nothing
        pure Prospect {..}

  lift $ do
    nothingOrResult <- scrapeStringLikeT (responseBody response) eventScraper
    case nothingOrResult of
      Nothing -> logWarnN $ T.pack $ unwords ["Failed to scrape from", show (getUri request)]
      Just (externalEvent, mImageUri) -> do
        mExternalEventId <- importExternalEventWithMImage (externalEvent, mImageUri)
        forM_ mExternalEventId $ \externalEventId -> do
          mProspect <- scrapeStringLikeT (responseBody response) (prospectScraper (Entity externalEventId externalEvent))
          mapM_ importProspect mProspect
