{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Foundation
  ( module Salsa.Party.Web.Server.Foundation,
    module Salsa.Party.Web.Server.Foundation.NavBar,
    module Salsa.Party.Web.Server.Foundation.Auth,
    module Salsa.Party.Web.Server.Foundation.I18N,
    module Salsa.Party.Web.Server.Foundation.Yesod,
    module Salsa.Party.Web.Server.Foundation.App,
  )
where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Default
import Data.Fixed
import Data.Maybe
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import GHC.Generics (Generic)
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.Auth
import Salsa.Party.Web.Server.Foundation.I18N
import Salsa.Party.Web.Server.Foundation.NavBar
import Salsa.Party.Web.Server.Foundation.Yesod
import Salsa.Party.Web.Server.Static
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.ICalendar as ICal
import Text.Julius
import Yesod
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadR

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR favicon_ico

data Coordinates = Coordinates
  { coordinatesLat :: !Nano,
    coordinatesLon :: !Nano
  }
  deriving (Show, Eq, Generic)

instance Validity Coordinates

instance Validity Textarea where
  validate = validate . unTextarea

-- This could potentially be dangerous if a type is read than written
instance PathPiece (Fixed a) where
  fromPathPiece = fmap MkFixed . fromPathPiece
  toPathPiece (MkFixed i) = toPathPiece i

placeCoordinates :: Place -> Coordinates
placeCoordinates Place {..} = Coordinates {coordinatesLat = placeLat, coordinatesLon = placeLon}

insertPlace_ :: MonadIO m => Text -> Coordinates -> SqlPersistT m ()
insertPlace_ address coordinates = void $ insertPlace address coordinates

insertPlace :: MonadIO m => Text -> Coordinates -> SqlPersistT m (Entity Place)
insertPlace address Coordinates {..} =
  upsertBy
    (UniquePlaceQuery address)
    ( Place
        { placeLat = coordinatesLat,
          placeLon = coordinatesLon,
          placeQuery = address
        }
    )
    [ PlaceLat =. coordinatesLat,
      PlaceLon =. coordinatesLon
    ]

getPosterForParty :: MonadIO m => PartyId -> SqlPersistT m (Maybe CASKey)
getPosterForParty partyId = do
  keys <- E.select $
    E.from $ \(partyPoster `E.InnerJoin` image) -> do
      E.on (partyPoster E.^. PartyPosterImage E.==. image E.^. ImageId)
      E.where_ (partyPoster E.^. PartyPosterParty E.==. E.val partyId)
      pure (image E.^. ImageKey)
  pure $ E.unValue <$> listToMaybe keys

getPosterForSchedule :: MonadIO m => ScheduleId -> SqlPersistT m (Maybe CASKey)
getPosterForSchedule scheduleId = do
  keys <- E.select $
    E.from $ \(schedulePoster `E.InnerJoin` image) -> do
      E.on (schedulePoster E.^. SchedulePosterImage E.==. image E.^. ImageId)
      E.where_ (schedulePoster E.^. SchedulePosterSchedule E.==. E.val scheduleId)
      pure (image E.^. ImageKey)
  pure $ E.unValue <$> listToMaybe keys

getPosterForExternalEvent :: MonadIO m => ExternalEventId -> SqlPersistT m (Maybe CASKey)
getPosterForExternalEvent externalEventId = do
  keys <- E.select $
    E.from $ \(externalEventPoster `E.InnerJoin` image) -> do
      E.on (externalEventPoster E.^. ExternalEventPosterImage E.==. image E.^. ImageId)
      E.where_ (externalEventPoster E.^. ExternalEventPosterExternalEvent E.==. E.val externalEventId)
      pure (image E.^. ImageKey)
  pure $ E.unValue <$> listToMaybe keys

deleteUserCompletely :: MonadIO m => UserId -> SqlPersistT m ()
deleteUserCompletely userId = do
  organiserIds <- selectKeysList [OrganiserUser ==. userId] [Asc OrganiserId]
  mapM_ deleteOrganiserCompletely organiserIds
  delete userId

deleteOrganiserCompletely :: MonadIO m => OrganiserId -> SqlPersistT m ()
deleteOrganiserCompletely organiserId = do
  partyIds <- selectKeysList [PartyOrganiser ==. organiserId] [Asc PartyId]
  mapM_ deletePartyCompletely partyIds
  scheduleIds <- selectKeysList [ScheduleOrganiser ==. organiserId] [Asc ScheduleId]
  mapM_ deleteScheduleCompletely scheduleIds
  delete organiserId

deletePartyCompletely :: MonadIO m => PartyId -> SqlPersistT m ()
deletePartyCompletely partyId = do
  deleteWhere [PartyPosterParty ==. partyId]
  delete partyId

deleteScheduleCompletely :: MonadIO m => ScheduleId -> SqlPersistT m ()
deleteScheduleCompletely scheduleId = do
  -- deleteWhere [SchedulePosterSchedule ==. scheduleId] TODO delete poster
  delete scheduleId

appDB :: (MonadReader App m, MonadLoggerIO m) => SqlPersistT (LoggingT IO) a -> m a
appDB func = do
  pool <- asks appConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool func pool) logFunc

newtype JSONLDData = JSONLDData {unJSONLDData :: Value}

instance HasContentType JSONLDData where
  getContentType _ = typeLD

instance ToContent JSONLDData where
  toContent = toContent . unJSONLDData

instance ToTypedContent JSONLDData where
  toTypedContent ldData = TypedContent typeLD $ toContent ldData

instance ToWidgetHead App JSONLDData where
  toWidgetHead (JSONLDData v) =
    toWidgetHead $
      H.script ! HA.type_ "application/ld+json" $
        H.lazyText $ renderJavascript $ toJavascript v

typeLD :: ContentType
typeLD = "application/ld+json"

toJSONLDData :: ToJSON a => a -> JSONLDData
toJSONLDData = JSONLDData . toJSON

instance HasContentType ICal.VCalendar where
  getContentType _ = typeCalendar

instance ToContent ICal.VCalendar where
  toContent = toContent . ICal.printICalendar def

instance ToTypedContent ICal.VCalendar where
  toTypedContent vCalendar = TypedContent typeCalendar $ toContent vCalendar

typeCalendar :: ContentType
typeCalendar = "text/calendar"
