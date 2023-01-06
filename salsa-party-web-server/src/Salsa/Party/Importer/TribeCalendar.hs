{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- A calendar from https://theeventscalendar.com/
--
-- This is a wordpress plugin:
-- https://theeventscalendar.com/products/wordpress-events-calendar/
--
-- At a tribe calendar url, you can add
-- /list/?tribe-bar-date=YYYY-MM-DD&ical=1
-- to the end to get an ICS file
--
-- This ICS file can then contain a bunch of events and those have URLs, which we can try to crawl.
--
-- Developer docs here:
-- https://theeventscalendar.com/knowledgebase/k/new-user-primer-the-events-calendar-and-events-calendar-pro/
module Salsa.Party.Importer.TribeCalendar
  ( tribeCalendarC,
    importTribeCalendarJSONLDEvents,
    tribeCalendarJSONLDEvents,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel
import qualified Web.JSONLD as LD

-- For a given URL, find the tribe calendar and get all the event URLs in there.
tribeCalendarC :: ConduitT URI URI Import ()
tribeCalendarC =
  andDays
    .| C.concatMap makeCalendarRequest
    .| httpRequestC
    .| httpBodyTextParserC
    .| parseUrlsInCalendars
    .| deduplicateC

-- | Make (but don't send) a calendar request for the given calendar URI and day.
makeCalendarRequest :: (URI, Day) -> Maybe HTTP.Request
makeCalendarRequest (uri, day) = do
  requestPrototype <- requestFromURI $ uri {uriPath = uriPath uri <> "/list/"}
  pure $
    setQueryString
      [ ("tribe-bar-date", Just $ TE.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%F" day),
        ("ical", Just "1")
      ]
      $ requestPrototype
        { requestHeaders = ("Accept", "application/calendar") : requestHeaders requestPrototype
        }

-- | Parse the URLs in an ICS file
--
-- Instead of parsing the actual ICS file, because they probably aren't valid
-- (I tried), we just take the URLs that are on a line that says "URL: "
parseUrlsInCalendars :: ConduitT (HTTP.Request, Response Text) URI Import ()
parseUrlsInCalendars =
  C.map (responseBody . snd)
    -- Unbounded is not safe here, but not sure what to do about it ..
    .| C.splitOnUnboundedE (== '\n')
    .| C.concatMap (T.stripPrefix "URL:")
    .| C.map (\lb -> fromMaybe lb $ T.stripSuffix "\r" lb) -- Strip \r if there is one.
    .| C.concatMap (parseURI . T.unpack)

importTribeCalendarJSONLDEvents :: ConduitT (LD.Event, (HTTP.Request, HTTP.Response Text)) Void Import ()
importTribeCalendarJSONLDEvents = tribeCalendarJSONLDEvents .| C.mapM_ importExternalEventWithMImage

tribeCalendarJSONLDEvents :: ConduitT (LD.Event, (HTTP.Request, HTTP.Response Text)) (ExternalEvent, Maybe URI) Import ()
tribeCalendarJSONLDEvents = awaitForever $ \(event, (request, response)) -> do
  -- We use this 'unescapeHtml' function because
  -- there are still html entities in the tags that we get.
  -- I'm not sure whether that's a mistake on their part or on ours, but it's definitely weird.
  externalEventUuid <- nextRandomUUID

  -- This is not ideal, because the URL could change, in which case we'll
  -- duplicate the event, but we don't have anything better it seems.
  let externalEventKey =
        let uriText = T.pack $ show $ getUri request
         in case T.stripPrefix "https://golatindance.com/event/" uriText of
              Nothing -> uriText
              Just suffix -> suffix
  let externalEventTitle = unescapeHtml $ LD.eventName event
  let externalEventDescription = scrapeStringLike (responseBody response) $
        chroot ("div" @: [hasClass "tribe-events-content"]) $ do
          rawHtmls <- htmls "p"
          let pScraper = do
                ls <- texts "p"
                pure $ T.intercalate "\n" ls
          -- We use forM_ instead of mayMaybe so that we never get partial descriptions
          ts <- forM rawHtmls $ \rawHtml -> case scrapeStringLike (T.replace "<br>" "" rawHtml) pScraper of
            Nothing -> fail "couldn't parse this tag"
            Just t -> pure t
          pure $ T.intercalate "\n\n" ts
  let externalEventOrganiser = do
        eventOrganizer <- LD.eventOrganizer event
        case eventOrganizer of
          LD.EventOrganizerOrganization organization -> pure $ LD.organizationName organization

  let (externalEventDay, externalEventStart) = case LD.eventStartDate event of
        LD.EventStartDate d -> (LD.dateDay d, Nothing)
        LD.EventStartDateTime dateTime ->
          let LocalTime d tod = LD.dateTimeLocalTime dateTime
           in (d, Just tod)
  today <- liftIO $ utctDay <$> getCurrentTime
  -- If the event is in the past, don't import it.
  -- We add '-1' to today to be safe with timezones that are way behind UTC.
  if externalEventDay < addDays (-1) today
    then pure ()
    else do
      let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
      -- It's probably possible to find this on the event page, but not in the event LD
      let externalEventHomepage = scrapeStringLike (responseBody response) $ chroot ("dd" @: [hasClass "tribe-events-event-url"]) $ attr "href" "a"

      -- Nowhere on the page as far as we can tell.
      let externalEventPrice = Nothing
      -- TODO the events may contain an attendance mode but in this case they don't seem to.
      -- We may want to try and parse it anyway in case that changes or we use this function somewhere else.
      let externalEventCancelled = Nothing
      let externalEventPoster = Nothing
      now <- liftIO getCurrentTime
      let externalEventCreated = now
      let externalEventModified = Nothing
      mPlaceEntity <- lift $ geocodeLDEventLocation $ LD.eventLocation event
      let mImageURI = do
            eventImage <- listToMaybe (LD.eventImages event)
            case eventImage of
              LD.EventImageURL t -> parseURI $ T.unpack t
      case mPlaceEntity of
        Nothing -> logWarnN "Place not found."
        Just (Entity externalEventPlace _) -> do
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request
          yield (ExternalEvent {..}, mImageURI)
