{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Salsa.Party.Web.Server.Handler.Import
import qualified Web.JSONLD as LD

getPartyR :: EventUUID -> Handler Html
getPartyR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just partyEntity -> partyPage partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Nothing -> notFound
        Just externalEventEntity -> externalEventPage externalEventEntity

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
  withNavBar $ do
    setTitle $ toHtml partyTitle
    setDescription $ fromMaybe "Party without description" partyDescription
    toWidgetHead $ toJSONLDData $ partyToLDEvent renderUrl party organiser place mPosterKey
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    $(widgetFile "party")

partyToLDEvent :: (Route App -> Text) -> Party -> Organiser -> Place -> Maybe CASKey -> LD.Event
partyToLDEvent renderUrl Party {..} Organiser {..} Place {..} mPosterKey =
  LD.Event
    { LD.eventName = partyTitle,
      LD.eventLocation =
        LD.EventLocationPlace $
          LD.Place
            { LD.placeName = Nothing,
              LD.placeAddress = LD.PlaceAddressText placeQuery
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

getImageR :: CASKey -> Handler TypedContent
getImageR key = do
  mImage <- runDB $ getBy $ UniqueImageKey key
  case mImage of
    Nothing -> notFound
    Just (Entity _ Image {..}) -> do
      -- Cache forever because of CAS
      addHeader "Cache-Control" "max-age=31536000, public, immutable"
      addHeader "Content-Disposition" "inline"
      setEtag $ renderCASKey key
      respond (TE.encodeUtf8 imageTyp) imageBlob

externalEventPage :: Entity ExternalEvent -> Handler Html
externalEventPage (Entity _ externalEvent@ExternalEvent {..}) = do
  place@Place {..} <- runDB $ get404 externalEventPlace
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
  withNavBar $ do
    setTitle $ toHtml externalEventTitle
    setDescription $ fromMaybe "Party without description" externalEventDescription
    toWidgetHead $ toJSONLDData $ externalEventToLDEvent externalEvent place
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe externalEventCreated externalEventModified
    $(widgetFile "external-event")

externalEventToLDEvent :: ExternalEvent -> Place -> LD.Event
externalEventToLDEvent ExternalEvent {..} Place {..} =
  LD.Event
    { LD.eventName = externalEventTitle,
      LD.eventLocation =
        LD.EventLocationPlace $
          LD.Place
            { LD.placeName = Nothing,
              LD.placeAddress = LD.PlaceAddressText placeQuery
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
      LD.eventEndDate = Nothing,
      LD.eventAttendanceMode = Just LD.OfflineEventAttendanceMode,
      LD.eventStatus =
        Just $
          if externalEventCancelled
            then LD.EventCancelled
            else LD.EventScheduled,
      LD.eventImages = [],
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
