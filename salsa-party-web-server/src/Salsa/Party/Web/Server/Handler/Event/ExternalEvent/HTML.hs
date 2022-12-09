{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.HTML (externalEventPageHtml) where

import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LT
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Google.Calendar
import Google.Maps
import qualified ICal
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Description
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPageHtml :: ExternalEvent -> Place -> Handler Html
externalEventPageHtml externalEvent@ExternalEvent {..} place@Place {..} = do
  isAdmin <- do
    mAuth <- maybeAuth
    case mAuth of
      Nothing -> pure False
      Just (Entity _ u) -> do
        mAdmin <- getsYesod appAdmin
        pure $ Just (userEmailAddress u) == mAdmin

  googleMapsWidget <- makeGoogleMapsWidget externalEventUuid placeQuery
  now <- getCurrentTimeH
  let today = utctDay now
  renderUrl <- getUrlRender
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  prettyDateTimeFormat <- getPrettyDateTimeFormat
  prettyTimeFormat <- getPrettyTimeFormat
  messageRender <- getMessageRender
  let guessedDanceStyles = guessDanceStyles externalEventTitle `S.union` maybe S.empty guessDanceStyles externalEventDescription
  withNavBar $ do
    setTitleI $ externalEventTitleMessage externalEvent
    setDescription $ externalEventHtmlDescription messageRender timeLocale prettyDayFormat prettyTimeFormat externalEvent place
    toWidgetHead $ toJSONLDData $ externalEventToLDEvent renderUrl externalEvent place
    addHeader "Last-Modified" $ TE.decodeLatin1 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe externalEventCreated externalEventModified
    addHeader "X-Robots-Tag" $ T.pack $ "unavailable_after: " <> formatTime timeLocale "%F" (addDays daysToKeepPartiesMarkedAsAvailable externalEventDay)
    let mAddToGoogleLink = addExternalEventToGoogleCalendarLink renderUrl externalEvent place
    let mHomepageLink = externalEventHomepage >>= (parseURILike . T.unpack)
    $(widgetFile "external-event")

parseURILike :: String -> Maybe URI
parseURILike url = parseAbsoluteURI url <|> parseAbsoluteURI ("https://" <> url)

addExternalEventToGoogleCalendarLink :: (Route App -> Text) -> ExternalEvent -> Place -> Maybe URI
addExternalEventToGoogleCalendarLink renderUrl externalEvent@ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (externalEventRoute externalEvent)) externalEventDay externalEventStart placeQuery externalEventTitle externalEventDescription
