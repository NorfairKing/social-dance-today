{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Default
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import qualified Text.HTML.TagSoup as HTML
import Text.ICalendar.Parser as ICal
import qualified Web.JSONLD as LD

golatindanceComImporter :: Importer
golatindanceComImporter =
  Importer
    { importerName = "golatindance.com",
      importerFunc = func
    }

logname = "Importer-golatindance.com"

func :: Import ()
func = do
  runConduit $
    yieldMany categories
      .| C.concatMap makeCalendarRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseUrlsInCalendars
      .| C.concatMap makeEventPageRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseJSONLDPieces
      .| parseJSONLDEvents
      .| importJSONLDEvents
      .| C.mapM_ (liftIO . print)

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

makeCalendarRequest :: Text -> Maybe HTTP.Request
makeCalendarRequest city = do
  let baseUrl = "https://golatindance.com"
      categoryIcalUrl category = baseUrl <> "/events/category/" <> category <> "/list"
  requestPrototype <- parseRequest $ categoryIcalUrl "london"
  pure $ setQueryString [("tribe-bar-date", Just "2021-07-21"), ("ical", Just "1")] $ requestPrototype {requestHeaders = ("Accept", "application/calendar") : requestHeaders requestPrototype}

logRequestErrors ::
  ConduitT
    (HTTP.Request, Either HttpException (Response LB.ByteString))
    (HTTP.Request, Response LB.ByteString)
    Import
    ()
logRequestErrors = awaitForever $ \(request, errOrResponse) -> case errOrResponse of
  Left err -> logErrorNS logname $ T.pack $ unlines ["Error while fetching calendar page: " <> ppShow err]
  Right response -> yield (request, response)

parseUrlsInCalendars :: ConduitT (HTTP.Request, Response LB.ByteString) Text Import ()
parseUrlsInCalendars =
  C.map (responseBody . snd)
    -- Unbounded is not safe here, but not sure what to do about it ..
    .| C.splitOnUnboundedE (== 0x0a)
    .| C.concatMap (LB.stripPrefix "URL:")
    .| C.map (\lb -> fromMaybe lb $ LB.stripSuffix "\r" lb) -- Strip \r if there is one.
    .| C.map LB.toStrict
    .| C.concatMap TE.decodeUtf8'

makeEventPageRequest :: Text -> Maybe HTTP.Request
makeEventPageRequest url = do
  guard $ T.isPrefixOf "https://golatindance.com/event/" url
  parseRequest $ T.unpack url

parseJSONLDPieces :: ConduitT (Request, Response LB.ByteString) (Request, JSON.Value) Import ()
parseJSONLDPieces = C.concatMap $ \(request, response) -> do
  let c = HTTP.statusCode (responseStatus response)
  guard $ 200 <= c && c < 300
  let pieces = groupIntoJSONLDPieces $ HTML.parseTags $ responseBody response
  -- Newer bytestring libraries actually have more efficient versions of
  -- LB8.dropWhile Char.isSpace
  -- Indeed what we want is a LB.strip instead.
  let bytestrings = map (LB8.dropWhile Char.isSpace . HTML.innerText) pieces
  value <- mapMaybe JSON.decode bytestrings
  pure (request, value)

groupIntoJSONLDPieces :: forall str. (Eq str, IsString str) => [HTML.Tag str] -> [[HTML.Tag str]]
groupIntoJSONLDPieces = lookForStart
  where
    lookForStart :: [HTML.Tag str] -> [[HTML.Tag str]]
    lookForStart stream = case dropWhile (not . isStartingTag) stream of
      [] -> [] -- Stop looking because there is no starting tag.
      (_startingTag : rest) -> lookForEnd rest
    lookForEnd :: [HTML.Tag str] -> [[HTML.Tag str]]
    lookForEnd stream =
      let (pieces, rest) = break isEndingTag stream
       in case pieces of
            [] -> [] -- Stop looking.
            _ -> case rest of
              [] -> [pieces] -- Stop looking
              (_endingTag : restrest) -> pieces : lookForStart restrest
    isStartingTag = \case
      HTML.TagOpen "script" attributes -> ("type", "application/ld+json") `elem` attributes
      _ -> False
    isEndingTag = \case
      HTML.TagClose "script" -> True
      _ -> False

parseJSONLDEvents ::
  ConduitT
    (HTTP.Request, JSON.Value)
    (HTTP.Request, LD.Event)
    Import
    ()
parseJSONLDEvents = awaitForever $ \(uri, value) ->
  case ((: []) <$> JSON.parseEither parseJSON value) <|> JSON.parseEither parseJSON value of
    Left err ->
      -- TODO check for the type before we start to try parsing?
      -- We don't need to log errors for every error because some are expected.
      logErrorNS logname $
        T.pack $
          unlines
            [ unwords ["Unable to parse JSON value as JSONLD Event:", err],
              ppShow value
            ]
    Right event -> yieldMany $ map ((,) uri) event

importJSONLDEvents :: ConduitT (HTTP.Request, LD.Event) () Import ()
importJSONLDEvents = awaitForever $ \(request, event) -> do
  liftIO $ print event
  externalEventUuid <- nextRandomUUID
  -- This is not ideal, but we don't have anything better it seems.
  let externalEventKey = T.pack $ show $ getUri request
  let externalEventTitle = LD.eventName event
  let externalEventDescription =
        -- Get rid of empty descriptions.
        case fromMaybe "" $ LD.eventDescription event of
          "" -> Nothing
          t -> Just t
  -- TODO the events MAY contain an organisers but in this case they don't seem to.
  -- We may want to try and parse it anyway in case that changes or we use this function somewhere else.
  let externalEventOrganiser = Nothing
  let (externalEventDay, externalEventStart) = case LD.eventStartDate event of
        LD.EventStartDate d -> (d, Nothing)
        LD.EventStartDateTime dateTime ->
          let LocalTime d tod = LD.dateTimeLocalTime dateTime
           in (d, Just tod)
  -- It's probably possible to find this on the event page, but not in the event LD
  let externalEventHomepage = Nothing
  -- It's probably possible to find this on the event page, but not in the event LD
  let externalEventPrice = Nothing
  -- TODO the events may contain an attendance mode but in this case they don't seem to.
  -- We may want to try and parse it anyway in case that changes or we use this function somewhere else.
  let externalEventCancelled = False
  now <- liftIO getCurrentTime
  let externalEventCreated = now
  let externalEventModified = Nothing
  Entity externalEventPlace _ <- case LD.eventLocation event of
    LD.EventLocationPlace place ->
      let address = case LD.placeAddress place of
            LD.PlaceAddressText t -> t
            LD.PlaceAddressPostalAddress postalAddress ->
              T.unwords $
                catMaybes
                  [ LD.postalAddressStreetAddress postalAddress,
                    LD.postalAddressLocality postalAddress,
                    LD.postalAddressRegion postalAddress,
                    LD.postalAddressCountry postalAddress
                  ]
       in case LD.placeGeo place of
            Just (LD.PlaceGeoCoordinates geoCoordinates) ->
              lift $
                importDB $
                  upsertBy
                    (UniquePlaceQuery address)
                    ( Place
                        { placeQuery = address,
                          placeLat = LD.geoCoordinatesLatitude geoCoordinates,
                          placeLon = LD.geoCoordinatesLongitude geoCoordinates
                        }
                    )
                    [] -- Don't change if it's already there, so that they can't fill our page with junk.
  externalEventImporter <- Just <$> asks importEnvId
  let externalEventOrigin = T.pack $ show $ getUri request
  lift $ importExternalEvent ExternalEvent {..}
