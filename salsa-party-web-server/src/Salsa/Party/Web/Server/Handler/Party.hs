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
          "name" .= htmlEscapedText partyTitle,
          "startDate" .= partyDay,
          "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
          "eventStatus"
            .= if partyCancelled
              then ("https://schema.org/EventCancelled" :: Text)
              else ("https://schema.org/EventScheduled" :: Text),
          "location"
            .= object
              [ "@type" .= ("Place" :: Text),
                "address" .= htmlEscapedText placeQuery
              ],
          "image"
            .= [renderUrl (ImageR posterKey) | posterKey <- maybeToList mPosterKey],
          "organizer"
            .= object
              [ "@type" .= ("Organization" :: Text),
                "name" .= htmlEscapedText organiserName,
                "url" .= renderUrl (OrganiserR organiserUuid)
              ]
        ],
        ["description" .= htmlEscapedText description | description <- maybeToList partyDescription]
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
          "name" .= htmlEscapedText externalEventTitle,
          "startDate" .= externalEventDay,
          "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
          "eventStatus"
            .= if externalEventCancelled
              then ("https://schema.org/EventCancelled" :: Text)
              else ("https://schema.org/EventScheduled" :: Text),
          "location"
            .= object
              [ "@type" .= ("Place" :: Text),
                "address" .= htmlEscapedText placeQuery
              ]
        ],
        [ "organizer"
            .= object
              [ "@type" .= ("Organization" :: Text),
                "name" .= htmlEscapedText organizer
              ]
          | organizer <- maybeToList externalEventOrganiser
        ],
        ["description" .= htmlEscapedText description | description <- maybeToList externalEventDescription]
      ]

htmlEscapedText :: Text -> Text
htmlEscapedText = LT.toStrict . HT.renderHtml . toHtml

newtype JSONLDData = JSONLDData Value

toJSONLDData :: ToJSON a => a -> JSONLDData
toJSONLDData = JSONLDData . toJSON

instance ToWidgetHead App JSONLDData where
  toWidgetHead (JSONLDData v) =
    toWidgetHead $
      H.script ! HA.type_ "application/ld+json" $
        H.preEscapedLazyText $ renderJavascript $ toJavascript v
