{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer.SalsaCH where

import Conduit
import Control.Concurrent.TokenLimiter
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.Conduit.Combinators as C
import Data.Fixed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI as URI
import Salsa.Party.Importer.Import
import System.Random

runSalsaCHImporter :: Importer
runSalsaCHImporter =
  Importer
    { importerName = "events.info",
      importerFunc = func
    }

-- There are events on https://events.info (the same events as on salsa.ch, it seems.
--
-- These events can be either parties or courses.
--
-- You can just fetch the page using the "Accept: application/json" header and get them back in json, super easy.
--
-- You'll get the events for today and tomorrow, but you can set the last_date query parameter to a %F date to
-- get the events for a given date.
--
-- (For a given date, not up to a given date, because it's implemented for infinite scrolling.
-- The page you already have all events until that date.)
--
-- In the result, we find whether the event is a course under "is_course" (false means party)
-- and whether it's been cancelled under "is_cancelled".
--
-- After that, you can look up events.info/events/:id to get the info about a specific event.
func :: Import ()
func = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let days = [today, addDays 2 today .. addDays 5 today]
  runConduit $
    yieldMany days
      .| homePageConduit
      .| eventPageConduit
      .| toExternalEvent
      .| externalEventSink

homePageConduit ::
  ConduitT
    Day
    Text -- EventId
    Import
    ()
homePageConduit =
  C.mapM makeHomepageRequest
    .| jsonRequestConduit
    .| C.concatMap
      ( map eventFromHomepageId
          . filter
            -- No courses.
            (not . eventFromHomepageIsCourse)
      )

makeHomepageRequest :: Day -> Import Request
makeHomepageRequest day = do
  let baseUrl = "https://events.info/"
  requestPrototype <- parseRequest baseUrl -- TODO this can fail, make that possible.
  pure $
    setQueryString
      [("last_date", Just $ TE.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%F" day)]
      $ requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}

data EventFromHomepage = EventFromHomepage
  { eventFromHomepageIsCourse :: !Bool,
    eventFromHomepageIsCancelled :: !Bool,
    eventFromHomepageId :: !Text
  }
  deriving (Show)

instance FromJSON EventFromHomepage where
  parseJSON = withObject "EventFromHomepage" $ \o ->
    EventFromHomepage
      <$> o .: "is_course"
      <*> o .: "is_cancelled"
      <*> o .: "id"

eventPageConduit :: ConduitT Text EventDetails Import ()
eventPageConduit = C.mapM makeEventPageRequest .| jsonRequestConduit

makeEventPageRequest :: Text -> Import Request
makeEventPageRequest identifier = do
  let baseUrl = "https://events.info/events/" <> T.unpack identifier
  requestPrototype <- parseRequest baseUrl -- TODO this can fail, make that ok.
  pure $ requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}

data EventDetails = EventDetails
  { eventDetailsId :: !Text,
    eventDetailsName :: !Text,
    eventDetailsDescription :: !(Maybe Text),
    eventDetailsCancelled :: !Bool,
    eventDetailsStart :: !ZonedTime,
    eventDetailsVenue :: !EventVenue,
    eventDetailsImages :: ![EventImage]
  }
  deriving (Show)

instance FromJSON EventDetails where
  parseJSON = withObject "EventDetails" $ \o ->
    EventDetails
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "description"
      <*> o .: "is_cancelled"
      <*> o .: "start_datetime"
      <*> o .: "venue"
      <*> o .:? "images" .!= []

data EventVenue = EventVenue
  { eventVenueName :: !(Maybe Text),
    eventVenueLocation :: !VenueLocation
  }
  deriving (Show, Eq)

instance FromJSON EventVenue where
  parseJSON = withObject "EventVenue" $ \o ->
    EventVenue
      <$> o .:? "name"
      <*> o .: "location"

data VenueLocation = VenueLocation
  { venueLocationCity :: !Text,
    venueLocationStreet :: !Text,
    venueLocationLat :: !Nano,
    venueLocationLon :: !Nano
  }
  deriving (Show, Eq)

instance FromJSON VenueLocation where
  parseJSON = withObject "VenueLocation" $ \o -> do
    venueLocationCity <- o .: "city"
    venueLocationStreet <- o .: "street"
    latLonList <- o .: "latlng"
    case latLonList of
      [venueLocationLat, venueLocationLon] -> pure VenueLocation {..}
      _ -> fail $ "Not exactly two coordinates: " <> show latLonList

data EventImage = EventImage
  { eventImageId :: !Text,
    eventImageSrc :: !URI
  }
  deriving (Show)

instance FromJSON EventImage where
  parseJSON = withObject "EventImage" $ \o ->
    EventImage
      <$> o .: "id"
      <*> o .: "src"

jsonRequestConduit :: FromJSON a => ConduitT Request a Import ()
jsonRequestConduit = do
  man <- asks appHTTPManager
  let limitConfig =
        defaultLimitConfig
          { maxBucketTokens = 10, -- Ten tokens maximum, represents one request
            initialBucketTokens = 10,
            bucketRefillTokensPerSecond = 1
          }
  rateLimiter <- liftIO $ newRateLimiter limitConfig
  awaitForever $ \requestPrototype -> do
    liftIO $ waitDebit limitConfig rateLimiter 10 -- Need 10 tokens
    userAgent <- liftIO chooseUserAgent
    let request = requestPrototype {requestHeaders = ("User-Agent", userAgent) : requestHeaders requestPrototype}
    logInfoNS "Importer" $ "fetching: " <> T.pack (show (getUri request))
    response <- liftIO $ httpLbs request man -- TODO this can fail, make that ok.
    let body = responseBody response
    case JSON.eitherDecode body of
      Left err ->
        logErrorNS "Importer" $
          T.unlines
            [ "Invalid JSON:" <> T.pack err,
              T.pack (show body)
            ]
      Right jsonValue ->
        case JSON.parseEither parseJSON jsonValue of
          Left err ->
            logErrorNS "Importer" $
              T.unlines
                [ "Unable to parse JSON:" <> T.pack err,
                  T.pack $ ppShow jsonValue
                ]
          Right a -> yield a

chooseUserAgent :: IO ByteString
chooseUserAgent = do
  index <- randomRIO (0, length userAgentList)
  pure $ userAgentList !! index

userAgentList :: [ByteString]
userAgentList =
  [ "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FSL 7.0.6.01001)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FSL 7.0.7.01001)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FSL 7.0.5.01003)",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0",
    "Mozilla/5.0 (X11; U; Linux x86_64; de; rv:1.9.2.8) Gecko/20100723 Ubuntu/10.04 (lucid) Firefox/3.6.8",
    "Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:11.0) Gecko/20100101 Firefox/11.0",
    "Mozilla/5.0 (X11; U; Linux x86_64; de; rv:1.9.2.8) Gecko/20100723 Ubuntu/10.04 (lucid) Firefox/3.6.8",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.0.3705)",
    "Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:13.0) Gecko/20100101 Firefox/13.0.1",
    "Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)",
    "Opera/9.80 (Windows NT 5.1; U; en) Presto/2.10.289 Version/12.01",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727)",
    "Mozilla/5.0 (Windows NT 5.1; rv:5.0.1) Gecko/20100101 Firefox/5.0.1",
    "Mozilla/5.0 (Windows NT 6.1; rv:5.0) Gecko/20100101 Firefox/5.02",
    "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.112 Safari/535.1",
    "Mozilla/4.0 (compatible; MSIE 6.0; MSIE 5.5; Windows NT 5.0) Opera 7.02 Bork-edition [en]"
  ]

toExternalEvent :: ConduitT EventDetails ExternalEvent Import ()
toExternalEvent = awaitForever $ \EventDetails {..} -> do
  let externalEventKey = eventDetailsId
  let externalEventTitle = eventDetailsName
  let externalEventDescription = eventDetailsDescription
  let externalEventOrganiser = eventVenueName eventDetailsVenue
  let LocalTime externalEventDay tod = zonedTimeToLocalTime eventDetailsStart
  let externalEventStart = Just tod
  let externalEventHomepage = Nothing
  let externalEventCancelled = eventDetailsCancelled
  now <- liftIO getCurrentTime
  let externalEventCreated = now
  let externalEventModified = Nothing
  let VenueLocation {..} = eventVenueLocation eventDetailsVenue
  let address = T.unwords [venueLocationStreet, venueLocationCity]
  Entity externalEventPlace _ <-
    appDB $
      upsertBy
        (UniquePlaceQuery address)
        (Place {placeQuery = address, placeLat = venueLocationLat, placeLon = venueLocationLon})
        [] -- Don't change if it's already there, so that they can't fill our page with junk.
  case parseAbsoluteURI $ "https://events.info/events/" <> T.unpack eventDetailsId of
    Nothing -> pure ()
    Just externalEventOrigin -> yield ExternalEvent {..}

externalEventSink :: ConduitT ExternalEvent Void Import ()
externalEventSink = awaitForever $ \externalEvent@ExternalEvent {..} -> do
  now <- liftIO getCurrentTime
  lift $
    appDB $ do
      mExternalEvent <- getBy (UniqueExternalEventKey externalEventKey)
      case mExternalEvent of
        Nothing -> insert_ externalEvent
        Just (Entity externalEventId oldExternalEvent) ->
          if externalEvent `hasChangedComparedTo` oldExternalEvent
            then
              void $
                update
                  externalEventId
                  [ ExternalEventTitle =. externalEventTitle,
                    ExternalEventDescription =. externalEventDescription,
                    ExternalEventOrganiser =. externalEventOrganiser,
                    ExternalEventDay =. externalEventDay,
                    ExternalEventStart =. externalEventStart,
                    ExternalEventHomepage =. externalEventHomepage,
                    ExternalEventModified =. Just now,
                    ExternalEventPlace =. externalEventPlace,
                    ExternalEventOrigin =. externalEventOrigin
                  ]
            else pure ()
