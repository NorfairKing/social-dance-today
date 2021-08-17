{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party
  ( partyPage,
  )
where

import qualified Data.Text.Encoding as TE
import Google.Calendar
import Network.HTTP.Types
import Network.URI
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPage :: Entity Party -> Handler TypedContent
partyPage partyEntity = selectRep $ do
  provideRep $ partyPageHtml partyEntity
  provideRep $ partyPageLD partyEntity

partyPageHtml :: Entity Party -> Handler Html
partyPageHtml (Entity partyId party@Party {..}) = do
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
    -- liftIO $ pPrint ("Place from db" :: Text, place)
    -- liftIO $ pPrint ("LD Event on server side" :: Text, ldEvent)
    toWidgetHead $ toJSONLDData ldEvent
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    let mAddToGoogleLink = addPartyToGoogleCalendarLink renderUrl party place
    $(widgetFile "party")

addPartyToGoogleCalendarLink :: (Route App -> Text) -> Party -> Place -> Maybe URI
addPartyToGoogleCalendarLink renderUrl Party {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in addEventToGoogleCalendarLink (renderUrl (EventR partyUuid)) partyDay partyStart placeQuery partyTitle partyDescription

partyPageLD :: Entity Party -> Handler JSONLDData
partyPageLD (Entity partyId party@Party {..}) = do
  place@Place {..} <- runDB $ get404 partyPlace
  organiser@Organiser {..} <- runDB $ get404 partyOrganiser
  mPosterKey <- runDB $ getPosterForParty partyId
  renderUrl <- getUrlRender
  pure $ toJSONLDData $ partyToLDEvent renderUrl party organiser place mPosterKey