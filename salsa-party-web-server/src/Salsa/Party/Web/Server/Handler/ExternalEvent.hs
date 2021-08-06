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
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types
import Network.URI
import Salsa.Party.Web.Server.Handler.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

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
          ("sprop", Just $ TE.encodeUtf8 $ "website:" <> renderUrl (EventR partyUuid))
        ]
  pure $ getUri $ setQueryString params requestPrototype
