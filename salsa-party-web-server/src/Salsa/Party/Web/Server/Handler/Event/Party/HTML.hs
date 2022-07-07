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

import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LT
import qualified Data.Text.Encoding as TE
import Google.Calendar
import Google.Maps
import qualified ICal
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.Party.Description
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPageHtml :: Entity Organiser -> Entity Party -> Handler Html
partyPageHtml (Entity _ organiser@Organiser {..}) (Entity partyId party@Party {..}) = do
  isAdmin <- do
    mAuth <- maybeAuth
    case mAuth of
      Nothing -> pure False
      Just (Entity _ u) -> do
        mAdmin <- getsYesod appAdmin
        pure $ Just (userEmailAddress u) == mAdmin

  place@Place {..} <- runDB $ get404 partyPlace
  mSchedule <- runDB $ getScheduleForParty partyId
  mPosterKey <- runDB $ getPosterForParty partyId
  googleMapsWidget <- makeGoogleMapsWidget partyUuid placeQuery
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
    addHeader "Last-Modified" $ TE.decodeLatin1 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    let mAddToGoogleLink = addPartyToGoogleCalendarLink renderUrl organiser party place
    $(widgetFile "party")

addPartyToGoogleCalendarLink :: (Route App -> Text) -> Organiser -> Party -> Place -> Maybe URI
addPartyToGoogleCalendarLink renderUrl organiser party@Party {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (partyRoute organiser party)) partyDay partyStart placeQuery partyTitle partyDescription
