{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Organiser.HTML
  ( getOrganiserR,
    getOrganiserSlugR,
  )
where

import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto.Legacy as E
import Safe (maximumMay)
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

getOrganiserR :: OrganiserUUID -> Handler Html
getOrganiserR uuid = do
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUUID uuid
  case mOrganiser of
    Nothing -> notFound
    Just organiserEntity@(Entity _ organiser) -> case organiserSlug organiser of
      Just slug -> redirect $ OrganiserSlugR slug
      Nothing -> organiserPage organiserEntity

getOrganiserSlugR :: OrganiserSlug -> Handler Html
getOrganiserSlugR slug = do
  mOrganiser <- runDB $ selectFirst [OrganiserSlug ==. Just slug] []
  case mOrganiser of
    Nothing -> notFound
    Just organiserEntity -> organiserPage organiserEntity

organiserPage :: Entity Organiser -> Handler Html
organiserPage (Entity organiserId organiser@Organiser {..}) = do
  today <- getClientToday
  parties <- runDB $ getUpcomingPartiesOfOrganiser today organiserId
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  prettyDateTimeFormat <- getPrettyDateTimeFormat
  let mLatestModifiedParty = maximumMay $ map (\(Entity _ Party {..}, _) -> fromMaybe partyCreated partyModified) parties
  let latestModifiedOrganiser = fromMaybe organiserCreated organiserModified
  let lastModified = maybe latestModifiedOrganiser (max latestModifiedOrganiser) mLatestModifiedParty
  withNavBar $ do
    setTitleI $ MsgOrganiserTitle organiserName
    setDescriptionI $ MsgOrganiserDescription organiserName
    renderUrl <- getUrlRender
    let ldEvents = map (\(Entity _ party, Entity _ place) -> partyToLDEvent renderUrl party organiser place) parties
    toWidgetHead $ toJSONLDData ldEvents
    addHeader "Last-Modified" $ TE.decodeLatin1 $ formatHTTPDate $ utcToHTTPDate lastModified
    addStylesheet $ StaticR zoom_without_container_css
    $(widgetFile "organiser") <> posterCSS

getUpcomingPartiesOfOrganiser :: MonadIO m => Day -> OrganiserId -> SqlPersistT m [(Entity Party, Entity Place)]
getUpcomingPartiesOfOrganiser today organiserId =
  E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
      E.where_ (party E.^. PartyDay E.>=. E.val today)
      E.orderBy [E.asc $ party E.^. PartyDay]
      pure (party, p)
