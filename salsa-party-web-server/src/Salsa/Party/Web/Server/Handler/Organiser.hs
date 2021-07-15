{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Organiser where

import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Handler.Import

getOrganiserR :: OrganiserUUID -> Handler Html
getOrganiserR uuid = do
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUUID uuid
  case mOrganiser of
    Nothing -> notFound
    Just (Entity organiserId organiser@Organiser {..}) -> do
      now <- liftIO getCurrentTime
      let today = utctDay now
      parties <- runDB $ getUpcomingPartiesOfOrganiser organiserId
      timeLocale <- getTimeLocale
      prettyDayFormat <- getPrettyDayFormat
      withNavBar $ do
        setTitle $ "Organiser profile: " <> toHtml organiserName
        setDescription $ mconcat ["The organiser profile of ", organiserName, ", and a list of their upcoming social dance parties"]
        addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe organiserCreated organiserModified
        $(widgetFile "organiser")

getUpcomingPartiesOfOrganiser :: MonadIO m => OrganiserId -> SqlPersistT m [(Entity Party, Entity Place, Maybe CASKey)]
getUpcomingPartiesOfOrganiser organiserId = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  partyTups <- E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
      E.where_ (party E.^. PartyDay E.>=. E.val today)
      E.orderBy [E.asc $ party E.^. PartyDay]
      pure (party, p)
  forM partyTups $ \(partyEntity@(Entity partyId _), placeEntity) -> do
    -- TODO this is potentially expensive, can we do it in one query?
    mKey <- getPosterForParty partyId
    pure (partyEntity, placeEntity, mKey)
