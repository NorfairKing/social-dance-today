{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.ExternalEvent
  ( externalEventPage,
  )
where

import qualified Data.Text.Encoding as TE
import Google.Calendar
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
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (EventR externalEventUuid)) externalEventDay externalEventStart placeQuery externalEventTitle externalEventDescription
