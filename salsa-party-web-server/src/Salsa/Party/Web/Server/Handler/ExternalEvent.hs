{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Handler.ExternalEvent
  ( externalEventPage,
    addEventToGoogleCalendarLink,
    addExternalEventToGoogleCalendarLink,
    externalEventCalendar,
    externalEventToLDEvent,
  )
where

import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Network.HTTP.Client
import Network.HTTP.Types
import Network.URI
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar as ICal
import qualified Web.JSONLD as LD

externalEventPage :: Entity ExternalEvent -> Handler Html
externalEventPage (Entity externalEventId externalEvent@ExternalEvent {..}) = do
  place@Place {..} <- runDB $ get404 externalEventPlace
  mPosterKey <- runDB $ getPosterForExternalEvent externalEventId
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  let mGoogleMapsEmbedUrl = do
        apiKey <- mGoogleAPIKey
        let mapsAPI = "https://www.google.com/maps/embed/v1/place"
        let googleMapsEmbedQuery =
              renderQuery
                True
                [ ("key", Just $ TE.encodeUtf8 apiKey),
                  ("q", Just $ TE.encodeUtf8 placeQuery)
                ]
        let googleMapsEmbedUrl = mapsAPI <> TE.decodeUtf8 googleMapsEmbedQuery
        pure googleMapsEmbedUrl
  now <- liftIO getCurrentTime
  let today = utctDay now
  renderUrl <- getUrlRender
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  prettyDateTimeFormat <- getPrettyDateTimeFormat
  withNavBar $ do
    setTitleI $
      if externalEventCancelled
        then MsgPartyTitleCancelled externalEventTitle
        else MsgPartyTitleScheduled externalEventTitle
    setDescriptionI $ maybe MsgPartyWithoutDescription MsgPartyDescription externalEventDescription
    toWidgetHead $ toJSONLDData $ externalEventToLDEvent renderUrl externalEvent place mPosterKey
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe externalEventCreated externalEventModified
    let mAddToGoogleLink = addExternalEventToGoogleCalendarLink renderUrl externalEvent place
    $(widgetFile "external-event")

externalEventToLDEvent :: (Route App -> Text) -> ExternalEvent -> Place -> Maybe CASKey -> LD.Event
externalEventToLDEvent renderUrl ExternalEvent {..} Place {..} mPosterKey =
  LD.Event
    { LD.eventName = externalEventTitle,
      LD.eventLocation =
        LD.EventLocationPlace $
          LD.Place
            { LD.placeName = Nothing,
              LD.placeAddress = LD.PlaceAddressText placeQuery,
              LD.placeGeo =
                Just $
                  LD.PlaceGeoCoordinates
                    LD.GeoCoordinates
                      { LD.geoCoordinatesLatitude = placeLat,
                        LD.geoCoordinatesLongitude = placeLon
                      }
            },
      LD.eventStartDate = case externalEventStart of
        Nothing -> LD.EventStartDate externalEventDay
        Just timeOfDay ->
          LD.EventStartDateTime
            LD.DateTime
              { dateTimeLocalTime =
                  LocalTime
                    { localDay = externalEventDay,
                      localTimeOfDay = timeOfDay
                    },
                dateTimeTimeZone = Nothing
              },
      LD.eventDescription = externalEventDescription,
      LD.eventUrl = Nothing,
      LD.eventEndDate = Nothing,
      LD.eventAttendanceMode = Just LD.OfflineEventAttendanceMode,
      LD.eventStatus =
        Just $
          if externalEventCancelled
            then LD.EventCancelled
            else LD.EventScheduled,
      LD.eventImages = [LD.EventImageURL (renderUrl (ImageR posterKey)) | posterKey <- maybeToList mPosterKey],
      LD.eventOrganizer = case externalEventOrganiser of
        Nothing -> Nothing
        Just name ->
          Just $
            LD.EventOrganizerOrganization
              LD.Organization
                { LD.organizationName = name,
                  organizationUrl = Nothing
                }
    }

addExternalEventToGoogleCalendarLink :: (Route App -> Text) -> ExternalEvent -> Place -> Maybe URI
addExternalEventToGoogleCalendarLink renderUrl ExternalEvent {..} Place {..} =
  addEventToGoogleCalendarLink renderUrl externalEventUuid externalEventDay externalEventStart placeQuery externalEventTitle externalEventDescription

addEventToGoogleCalendarLink :: (Route App -> Text) -> EventUUID -> Day -> Maybe TimeOfDay -> Text -> Text -> Maybe Text -> Maybe URI
addEventToGoogleCalendarLink renderUrl partyUuid partyDay partyStart placeQuery partyTitle partyDescription = do
  -- We go via request because URI doesn't have any functions for this.
  requestPrototype <- parseRequest "https://calendar.google.com/calendar/r/eventedit"
  -- Exlanation of this here:
  -- https://github.com/InteractionDesignFoundation/add-event-to-calendar-docs/blob/master/services/google.md
  let dayFormat = "%Y%m%d"
      localTimeFormat = "%Y%m%dT%H%M%S"
      formatDay :: Day -> String
      formatDay = formatTime defaultTimeLocale dayFormat
      formatLocalTime :: LocalTime -> String
      formatLocalTime = formatTime defaultTimeLocale localTimeFormat
  let startStr = case partyStart of
        Nothing -> formatDay partyDay
        Just start -> formatLocalTime $ LocalTime partyDay start
  let endStr = case partyStart of
        Nothing -> formatDay $ addDays 1 partyDay
        Just start ->
          formatLocalTime $
            let startTime = LocalTime partyDay start
             in addLocalTime (2 * 60 * 60) startTime -- Two hours later
  let datesString = TE.encodeUtf8 $ T.pack $ startStr <> "/" <> endStr
  let params =
        [ ("action", Just "TEMPLATE"),
          ("text", Just $ TE.encodeUtf8 partyTitle),
          ("dates", Just datesString),
          ("details", TE.encodeUtf8 <$> partyDescription),
          ("location", Just $ TE.encodeUtf8 placeQuery),
          ("sprop", Just $ TE.encodeUtf8 $ "website:" <> renderUrl (PartyR partyUuid))
        ]
  pure $ getUri $ setQueryString params requestPrototype

externalEventCalendar :: (Route App -> Text) -> ExternalEvent -> Place -> ICal.VCalendar
externalEventCalendar renderUrl externalEvent@ExternalEvent {..} place =
  def
    { ICal.vcEvents =
        M.singleton
          (LT.fromStrict $ uuidText externalEventUuid, Just dateTime)
          (externalEventCalendarEvent renderUrl externalEvent place)
    }
  where
    dateTime = case externalEventStart of
      Nothing -> Left $ ICal.Date externalEventDay
      Just start -> Right $ ICal.FloatingDateTime (LocalTime externalEventDay start)

externalEventCalendarEvent :: (Route App -> Text) -> ExternalEvent -> Place -> ICal.VEvent
externalEventCalendarEvent renderUrl ExternalEvent {..} Place {..} =
  let noOther = def
   in ICal.VEvent
        { ICal.veDTStamp =
            ICal.DTStamp
              { ICal.dtStampValue =
                  fromMaybe externalEventCreated externalEventModified,
                ICal.dtStampOther = noOther
              },
          ICal.veUID =
            ICal.UID
              { ICal.uidValue = LT.fromStrict $ uuidText externalEventUuid,
                ICal.uidOther = noOther
              },
          ICal.veClass =
            ICal.Class
              { ICal.classValue = ICal.Public,
                ICal.classOther = noOther
              },
          ICal.veDTStart = Just $ case externalEventStart of
            Nothing ->
              ICal.DTStartDate
                { ICal.dtStartDateValue = ICal.Date externalEventDay,
                  dtStartOther = noOther
                }
            Just start ->
              ICal.DTStartDateTime
                { ICal.dtStartDateTimeValue = ICal.FloatingDateTime (LocalTime externalEventDay start),
                  ICal.dtStartOther = noOther
                },
          ICal.veCreated =
            Just $
              ICal.Created
                { ICal.createdValue = externalEventCreated,
                  ICal.createdOther = noOther
                },
          ICal.veDescription =
            ( \description ->
                ICal.Description
                  { ICal.descriptionValue = LT.fromStrict description,
                    ICal.descriptionAltRep = Nothing,
                    ICal.descriptionLanguage = Nothing,
                    ICal.descriptionOther = noOther
                  }
            )
              <$> externalEventDescription,
          ICal.veGeo =
            Just $
              ICal.Geo
                { ICal.geoLat = realToFrac placeLat,
                  ICal.geoLong = realToFrac placeLon,
                  ICal.geoOther = noOther
                },
          ICal.veLastMod =
            ( \modified ->
                ICal.LastModified
                  { ICal.lastModifiedValue = modified,
                    ICal.lastModifiedOther = noOther
                  }
            )
              <$> externalEventModified,
          ICal.veLocation =
            Just $
              ICal.Location
                { ICal.locationValue = LT.fromStrict placeQuery,
                  ICal.locationAltRep = Nothing,
                  ICal.locationLanguage = Nothing,
                  ICal.locationOther = noOther
                },
          ICal.veOrganizer = Nothing,
          ICal.vePriority = def,
          ICal.veSeq = def,
          ICal.veStatus =
            Just $
              if externalEventCancelled
                then ICal.CancelledEvent {eventStatusOther = noOther}
                else ICal.ConfirmedEvent {eventStatusOther = noOther},
          ICal.veSummary =
            Just $
              ICal.Summary
                { ICal.summaryValue = LT.fromStrict externalEventTitle,
                  ICal.summaryAltRep = Nothing,
                  ICal.summaryLanguage = Nothing,
                  ICal.summaryOther = noOther
                },
          ICal.veTransp = ICal.Transparent {timeTransparencyOther = noOther},
          ICal.veUrl = do
            uri <- parseURI $ T.unpack $ renderUrl $ PartyR externalEventUuid
            pure $ ICal.URL {ICal.urlValue = uri, ICal.urlOther = noOther},
          ICal.veRecurId = Nothing,
          ICal.veRRule = S.empty,
          ICal.veDTEndDuration = Nothing,
          ICal.veAttach = S.empty,
          ICal.veAttendee = S.empty,
          ICal.veCategories = S.empty,
          ICal.veComment = S.empty,
          ICal.veContact = S.empty,
          ICal.veExDate = S.empty,
          ICal.veRStatus = S.empty,
          ICal.veRelated = S.empty,
          ICal.veResources = S.empty,
          ICal.veRDate = S.empty,
          ICal.veAlarms = S.empty,
          ICal.veOther = S.empty
        }
