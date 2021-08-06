{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Handler.Party
  ( partyPage,
    partyCalendar,
    partyToLDEvent,
  )
where

import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types
import Network.URI
import Salsa.Party.Web.Server.Handler.ExternalEvent
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar as ICal
import qualified Web.JSONLD as LD

partyPage :: Entity Party -> Handler Html
partyPage (Entity partyId party@Party {..}) = do
  place@Place {..} <- runDB $ get404 partyPlace
  organiser@Organiser {..} <- runDB $ get404 partyOrganiser
  mPosterKey <- runDB $ getPosterForParty partyId
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
      if partyCancelled
        then MsgPartyTitleCancelled partyTitle
        else MsgPartyTitleScheduled partyTitle
    setDescriptionI $ maybe MsgPartyWithoutDescription MsgPartyDescription partyDescription
    toWidgetHead $ toJSONLDData $ partyToLDEvent renderUrl party organiser place mPosterKey
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    let mAddToGoogleLink = addPartyToGoogleCalendarLink renderUrl party place
    $(widgetFile "party")

partyToLDEvent :: (Route App -> Text) -> Party -> Organiser -> Place -> Maybe CASKey -> LD.Event
partyToLDEvent renderUrl Party {..} Organiser {..} Place {..} mPosterKey =
  LD.Event
    { LD.eventName = partyTitle,
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
      LD.eventStartDate = case partyStart of
        Nothing -> LD.EventStartDate partyDay
        Just timeOfDay ->
          LD.EventStartDateTime
            LD.DateTime
              { dateTimeLocalTime =
                  LocalTime
                    { localDay = partyDay,
                      localTimeOfDay = timeOfDay
                    },
                dateTimeTimeZone = Nothing
              },
      LD.eventDescription = partyDescription,
      LD.eventUrl = Nothing,
      LD.eventEndDate = Nothing,
      LD.eventAttendanceMode = Just LD.OfflineEventAttendanceMode,
      LD.eventStatus =
        Just $
          if partyCancelled
            then LD.EventCancelled
            else LD.EventScheduled,
      LD.eventImages = [LD.EventImageURL (renderUrl (ImageR posterKey)) | posterKey <- maybeToList mPosterKey],
      LD.eventOrganizer =
        Just $
          LD.EventOrganizerOrganization
            LD.Organization
              { LD.organizationName = organiserName,
                organizationUrl = Just $ renderUrl (OrganiserR organiserUuid)
              }
    }

addPartyToGoogleCalendarLink :: (Route App -> Text) -> Party -> Place -> Maybe URI
addPartyToGoogleCalendarLink renderUrl Party {..} Place {..} =
  addEventToGoogleCalendarLink renderUrl partyUuid partyDay partyStart placeQuery partyTitle partyDescription

partyCalendar :: (Route App -> Text) -> Party -> Place -> ICal.VCalendar
partyCalendar renderUrl party@Party {..} place =
  def
    { ICal.vcEvents =
        M.singleton
          (LT.fromStrict $ uuidText partyUuid, Just dateTime)
          (partyCalendarEvent renderUrl party place)
    }
  where
    dateTime = case partyStart of
      Nothing -> Left $ ICal.Date partyDay
      Just start -> Right $ ICal.FloatingDateTime (LocalTime partyDay start)

partyCalendarEvent :: (Route App -> Text) -> Party -> Place -> ICal.VEvent
partyCalendarEvent renderUrl Party {..} Place {..} =
  let noOther = def
   in ICal.VEvent
        { ICal.veDTStamp =
            ICal.DTStamp
              { ICal.dtStampValue =
                  fromMaybe partyCreated partyModified,
                ICal.dtStampOther = noOther
              },
          ICal.veUID =
            ICal.UID
              { ICal.uidValue = LT.fromStrict $ uuidText partyUuid,
                ICal.uidOther = noOther
              },
          ICal.veClass =
            ICal.Class
              { ICal.classValue = ICal.Public,
                ICal.classOther = noOther
              },
          ICal.veDTStart = Just $ case partyStart of
            Nothing ->
              ICal.DTStartDate
                { ICal.dtStartDateValue = ICal.Date partyDay,
                  dtStartOther = noOther
                }
            Just start ->
              ICal.DTStartDateTime
                { ICal.dtStartDateTimeValue = ICal.FloatingDateTime (LocalTime partyDay start),
                  ICal.dtStartOther = noOther
                },
          ICal.veCreated =
            Just $
              ICal.Created
                { ICal.createdValue = partyCreated,
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
              <$> partyDescription,
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
              <$> partyModified,
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
              if partyCancelled
                then ICal.CancelledEvent {eventStatusOther = noOther}
                else ICal.ConfirmedEvent {eventStatusOther = noOther},
          ICal.veSummary =
            Just $
              ICal.Summary
                { ICal.summaryValue = LT.fromStrict partyTitle,
                  ICal.summaryAltRep = Nothing,
                  ICal.summaryLanguage = Nothing,
                  ICal.summaryOther = noOther
                },
          ICal.veTransp = ICal.Transparent {timeTransparencyOther = noOther},
          ICal.veUrl = do
            uri <- parseURI $ T.unpack $ renderUrl $ EventR partyUuid
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
