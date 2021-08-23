{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party
  ( partyPage,
    partyPageICal,
  )
where

import qualified Data.Text.Encoding as TE
import Google.Calendar
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPage :: Entity Party -> Handler TypedContent
partyPage partyEntity = selectRep $ do
  provideRep $ partyPageHtml partyEntity
  provideRep $ partyPageLD partyEntity
  provideRep $ partyPageICal partyEntity

partyPageHtml :: Entity Party -> Handler Html
partyPageHtml (Entity partyId party@Party {..}) = do
  place@Place {..} <- runDB $ get404 partyPlace
  organiser@Organiser {..} <- runDB $ get404 partyOrganiser
  mSchedule <- runDB $ getScheduleForParty partyId
  mPosterKey <- runDB $ getPosterForParty partyId
  mGoogleMapsEmbedUrl <- makeGoogleMapsEmbedUrl placeQuery
  now <- liftIO getCurrentTime
  let today = utctDay now
  renderUrl <- getUrlRender
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  prettyDateTimeFormat <- getPrettyDateTimeFormat
  withNavBar $ do
    setTitleI $
      if partyCancelled
        then MsgPartyTitleCancelled partyTitle
        else MsgPartyTitleScheduled partyTitle
    setDescriptionI $ maybe MsgPartyWithoutDescription MsgPartyDescription partyDescription
    let ldEvent = partyToLDEvent renderUrl party organiser place mPosterKey
    toWidgetHead $ toJSONLDData ldEvent
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    let mAddToGoogleLink = addPartyToGoogleCalendarLink renderUrl party place
    $(widgetFile "party")

addPartyToGoogleCalendarLink :: (Route App -> Text) -> Party -> Place -> Maybe URI
addPartyToGoogleCalendarLink renderUrl Party {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (EventR partyUuid)) partyDay partyStart placeQuery partyTitle partyDescription
