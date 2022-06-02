{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://dancefloorfinder.com
--
-- 1. There are no terms of service.
-- 2. The robots.txt does not forbid crawling.
--
-- This is a SPA, with a JSON API, extra easy!
--
-- Under https://dancefloorfinder.com/api/home, there is a list of cities.
--
-- Under https://dancefloorfinder.com/api/:city, there is a list of parties in this format:
--
--   {
--     "id": 0,
--     "date": "2021-09-12",
--     "title": "Oslo Cuban Salsa Bootcamp",
--     "dances": [
--       "salsa"
--     ],
--     "start_at": "11:00",
--     "end_at": "15:00",
--     "country": "Norway",
--     "city": "Oslo",
--     "price": "490",
--     "address": "Dancify Studio Eikenga 11, 0579 Oslo, Norwa",
--     "link": "https://www.facebook.com/events/4133405723372615",
--     "organization": null
--   }
--
--  Easy peasy.
--
--  Most of these contain a facebook event link, so not handy, but fine for now.
--
--  We'll put the "dances" list in the description but otherwise keep the description clean.
module Salsa.Party.Importer.DancefloorfinderCom (dancefloorfinderComImporter) where

import Conduit
import Data.Aeson as JSON
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding

dancefloorfinderComImporter :: Importer
dancefloorfinderComImporter =
  Importer
    { importerName = "dancefloorfinder.com",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "http://dancefloorfinder.com/api/home"
      .| C.concatMap (parseRequest :: String -> Maybe Request)
      .| jsonRequestConduit
      .| (C.concat :: ConduitT [String] String Import ())
      .| C.concatMap (\s -> parseRequest ("http://dancefloorfinder.com/api/" <> (s :: String)) :: Maybe Request)
      .| jsonRequestConduit'
      .| C.concatMap (\(request, events) -> (,) request <$> (events :: [Event]))
      .| importEventSync

data Event = Event
  { eventDay :: !Day,
    eventTitle :: !Text,
    eventAddress :: !Text,
    eventCity :: !(Maybe Text),
    eventCountry :: !(Maybe Text),
    eventDances :: ![Text],
    eventStart :: !(Maybe TimeOfDay),
    eventEnd :: !(Maybe TimeOfDay),
    eventLink :: !(Maybe Text),
    eventPrice :: !(Maybe Text),
    eventOrganisation :: !(Maybe Text)
  }
  deriving (Show, Eq)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    Event
      <$> o .: "date"
      <*> o .: "title"
      <*> o .: "address"
      <*> o .:? "city"
      <*> o .:? "country"
      <*> o .:? "dances" .!= []
      <*> o .:? "start_at"
      <*> ( do
              ms <- o .:? "end_at"
              fmap join $
                forM ms $ \s -> case s of
                  "" -> pure Nothing
                  _ -> parseJSON (JSON.String s)
          )
      <*> o .:? "link"
      <*> o .:? "price"
      <*> o .:? "organization"

importEventSync :: ConduitT (HTTP.Request, Event) void Import ()
importEventSync = awaitForever $ \(request, Event {..}) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now

  if today < addDays (-1) today
    then pure ()
    else do
      let title = T.strip eventTitle
      let externalEventKey = T.pack $ show (getUri request) <> formatTime defaultTimeLocale "%F" eventDay <> T.unpack title
      externalEventUuid <- nextRandomUUID
      let externalEventTitle = title
      let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
      let externalEventDescription = case eventDances of
            [] -> Nothing
            dances -> Just $ T.intercalate ", " dances
      let externalEventOrganiser = T.strip <$> eventOrganisation
      let externalEventDay = eventDay
      let externalEventStart = eventStart
      let externalEventHomepage = T.strip <$> eventLink
      let externalEventPrice = eventPrice
      -- There is no indication of whether something is cancelled.
      -- I guess it either dissappears or is just not updated.
      -- Either way, nothing we can do about that.
      let externalEventCancelled = Nothing
      let externalEventCreated = now
      let externalEventModified = Nothing

      let address = T.intercalate ", " $ map T.strip $ catMaybes [Just eventAddress, eventCity, eventCountry]

      app <- asks importEnvApp
      mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
      case mPlaceEntity of
        Nothing -> logDebugN $ "Could not geolocate: " <> address
        Just (Entity placeId _) -> do
          let externalEventPlace = placeId
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request

          lift $ importExternalEvent ExternalEvent {..}
