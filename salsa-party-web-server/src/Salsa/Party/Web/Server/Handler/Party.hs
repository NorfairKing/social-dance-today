{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Data.Aeson as JSON
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.Blaze.Html.Renderer.Text as HT
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Julius
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
    toWidgetHead $ toJSONLDData $ partyJSONLDData renderUrl party organiser place mPosterKey
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    $(widgetFile "party")

-- https://developers.google.com/search/docs/data-types/event
partyJSONLDData :: (Route App -> Text) -> Party -> Organiser -> Place -> Maybe CASKey -> JSON.Value
partyJSONLDData renderUrl Party {..} Organiser {..} Place {..} mPosterKey =
  object $
    concat
      [ [ "@context" .= ("https://schema.org" :: Text),
          "@type" .= ("Event" :: Text),
          "name" .= partyTitle,
          "startDate" .= partyDay,
          "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
          "eventStatus"
            .= if partyCancelled
              then ("https://schema.org/EventCancelled" :: Text)
              else ("https://schema.org/EventScheduled" :: Text),
          "location"
            .= object
              [ "@type" .= ("Place" :: Text),
                "address" .= placeQuery
              ],
          "image"
            .= [renderUrl (ImageR posterKey) | posterKey <- maybeToList mPosterKey],
          "organizer"
            .= object
              [ "@type" .= ("Organization" :: Text),
                "name" .= organiserName,
                "url" .= renderUrl (OrganiserR organiserUuid)
              ]
        ],
        ["description" .= description | description <- maybeToList partyDescription]
      ]

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
    toWidgetHead $ toJSONLDData $ externalEventJSONLDData externalEvent place
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe externalEventCreated externalEventModified
    $(widgetFile "external-event")

externalEventJSONLDData :: ExternalEvent -> Place -> JSON.Value
externalEventJSONLDData ExternalEvent {..} Place {..} =
  object $
    concat
      [ [ "@context" .= ("https://schema.org" :: Text),
          "@type" .= ("Event" :: Text),
          "name" .= externalEventTitle,
          "startDate" .= externalEventDay,
          "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
          "eventStatus"
            .= if externalEventCancelled
              then ("https://schema.org/EventCancelled" :: Text)
              else ("https://schema.org/EventScheduled" :: Text),
          "location"
            .= object
              [ "@type" .= ("Place" :: Text),
                "address" .= placeQuery
              ]
        ],
        [ "organizer"
            .= object
              [ "@type" .= ("Organization" :: Text),
                "name" .= organizer
              ]
          | organizer <- maybeToList externalEventOrganiser
        ],
        ["description" .= description | description <- maybeToList externalEventDescription]
      ]

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
                    }
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
