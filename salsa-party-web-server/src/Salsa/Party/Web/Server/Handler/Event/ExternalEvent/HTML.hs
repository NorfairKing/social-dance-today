{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.HTML (externalEventPageHtml) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Google.Calendar
import Google.Maps
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Description
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPageHtml :: Entity ExternalEvent -> Entity Place -> Maybe CASKey -> Handler Html
externalEventPageHtml (Entity _ externalEvent@ExternalEvent {..}) (Entity _ place@Place {..}) mPosterKey = do
  googleMapsWidget <- makeGoogleMapsWidget externalEventUuid placeQuery
  now <- getCurrentTimeH
  let today = utctDay now
  renderUrl <- getUrlRender
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  prettyDateTimeFormat <- getPrettyDateTimeFormat
  prettyTimeFormat <- getPrettyTimeFormat
  messageRender <- getMessageRender
  withNavBar $ do
    setTitleI $ externalEventTitleMessage externalEvent
    setDescription $ externalEventHtmlDescription messageRender timeLocale prettyDayFormat prettyTimeFormat externalEvent place
    toWidgetHead $ toJSONLDData $ externalEventToLDEvent renderUrl externalEvent place mPosterKey
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe externalEventCreated externalEventModified
    let mAddToGoogleLink = addExternalEventToGoogleCalendarLink renderUrl externalEvent place
    let mHomepageLink = externalEventHomepage >>= (parseURILike . T.unpack)
    $(widgetFile "external-event")

parseURILike :: String -> Maybe URI
parseURILike url = parseAbsoluteURI url <|> parseAbsoluteURI ("https://" <> url)

addExternalEventToGoogleCalendarLink :: (Route App -> Text) -> ExternalEvent -> Place -> Maybe URI
addExternalEventToGoogleCalendarLink renderUrl externalEvent@ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (externalEventRoute externalEvent)) externalEventDay externalEventStart placeQuery externalEventTitle externalEventDescription
