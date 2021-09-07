{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.HTML (partyPageHtml) where

import qualified Data.Text.Encoding as TE
import Google.Calendar
import Google.Maps
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.Party.Description
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPageHtml :: Entity Party -> Handler Html
partyPageHtml (Entity partyId party@Party {..}) = do
  place@Place {..} <- runDB $ get404 partyPlace
  organiser@Organiser {..} <- runDB $ get404 partyOrganiser
  mSchedule <- runDB $ getScheduleForParty partyId
  mPosterKey <- runDB $ getPosterForParty partyId
  mGoogleMapsWidget <- makeGoogleMapsWidget partyUuid placeQuery
  now <- getCurrentTimeH
  let today = utctDay now
  renderUrl <- getUrlRender
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  prettyDateTimeFormat <- getPrettyDateTimeFormat
  prettyTimeFormat <- getPrettyTimeFormat
  withNavBar $ do
    setTitleI $
      if partyCancelled
        then MsgPartyTitleCancelled partyTitle
        else MsgPartyTitleScheduled partyTitle
    messageRender <- getMessageRender
    setDescription $ partyHtmlDescription messageRender timeLocale prettyDayFormat prettyTimeFormat party organiser place
    let ldEvent = partyToLDEvent renderUrl party organiser place mPosterKey
    toWidgetHead $ toJSONLDData ldEvent
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    let mAddToGoogleLink = addPartyToGoogleCalendarLink renderUrl party place
    $(widgetFile "party")

addPartyToGoogleCalendarLink :: (Route App -> Text) -> Party -> Place -> Maybe URI
addPartyToGoogleCalendarLink renderUrl Party {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (EventR partyUuid)) partyDay partyStart placeQuery partyTitle partyDescription
