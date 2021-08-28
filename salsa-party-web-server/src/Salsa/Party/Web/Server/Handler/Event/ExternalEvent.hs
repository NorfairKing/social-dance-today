{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent (externalEventPage) where

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
  mGoogleMapsWidget <- makeGoogleMapsWidget $ Entity externalEventPlace place
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
