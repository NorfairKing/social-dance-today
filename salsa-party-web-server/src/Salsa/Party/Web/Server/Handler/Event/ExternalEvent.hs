{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent
  ( externalEventPage,
    externalEventHtmlDescription,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Google.Calendar
import Google.Maps
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPage :: Entity ExternalEvent -> Handler TypedContent
externalEventPage externalEventEntity = selectRep $ do
  provideRep $ externalEventPageHtml externalEventEntity
  provideRep $ externalEventPageLD externalEventEntity
  provideRep $ externalEventPageICal externalEventEntity

externalEventPageHtml :: Entity ExternalEvent -> Handler Html
externalEventPageHtml (Entity externalEventId externalEvent@ExternalEvent {..}) = do
  place@Place {..} <- runDB $ get404 externalEventPlace
  mPosterKey <- runDB $ getPosterForExternalEvent externalEventId
  mGoogleMapsWidget <- makeGoogleMapsWidget placeQuery
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
    let mHomepageLink = externalEventHomepage >>= (parseURILike . T.unpack)
    $(widgetFile "external-event")

parseURILike :: String -> Maybe URI
parseURILike url = parseAbsoluteURI url <|> parseAbsoluteURI ("https://" <> url)

addExternalEventToGoogleCalendarLink :: (Route App -> Text) -> ExternalEvent -> Place -> Maybe URI
addExternalEventToGoogleCalendarLink renderUrl ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (EventR externalEventUuid)) externalEventDay externalEventStart placeQuery externalEventTitle externalEventDescription

externalEventHtmlDescription :: (AppMessage -> Text) -> TimeLocale -> String -> String -> ExternalEvent -> Place -> Text
externalEventHtmlDescription render timeLocale prettyDayFormat prettyTimeFormat ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Organiser _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
   in T.unlines $
        concat
          [ [T.take 75 (render (MsgPartyDescription description)) | description <- maybeToList externalEventDescription],
            [ render
                ( case externalEventStart of
                    Nothing -> MsgPartyDescriptionDay $ formatTime timeLocale prettyDayFormat externalEventDay
                    Just start -> MsgPartyDescriptionDateTime (formatTime timeLocale prettyDayFormat externalEventDay) (formatTime timeLocale prettyTimeFormat start)
                ),
              render (MsgPartyDescriptionAddress placeQuery)
            ],
            [render (MsgPartyDescriptionOrganiser organiserName) | organiserName <- maybeToList externalEventOrganiser]
            -- We don't include the price because it's not going to be very relevant in search results
          ]
