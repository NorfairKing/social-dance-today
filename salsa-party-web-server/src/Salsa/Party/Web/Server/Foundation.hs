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
import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E
import Database.Persist.Sql
import Google.Maps
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
import Yesod
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadR

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR favicon_ico

instance Validity Textarea where
  validate = validate . unTextarea

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
getPosterForParty partyId =
  fmap (fmap E.unValue) $
    selectOne $
      E.from $ \(partyPoster `E.InnerJoin` image) -> do
        E.on (partyPoster E.^. PartyPosterImage E.==. image E.^. ImageId)
        E.where_ (partyPoster E.^. PartyPosterParty E.==. E.val partyId)
        pure (image E.^. ImageKey)

getPosterForSchedule :: MonadIO m => ScheduleId -> SqlPersistT m (Maybe CASKey)
getPosterForSchedule scheduleId =
  fmap (fmap E.unValue) $
    selectOne $
      E.from $ \(schedulePoster `E.InnerJoin` image) -> do
        E.on (schedulePoster E.^. SchedulePosterImage E.==. image E.^. ImageId)
        E.where_ (schedulePoster E.^. SchedulePosterSchedule E.==. E.val scheduleId)
        pure (image E.^. ImageKey)

getPosterForExternalEvent :: MonadIO m => ExternalEventId -> SqlPersistT m (Maybe CASKey)
getPosterForExternalEvent externalEventId = do
  keys <- E.select $
    E.from $ \(externalEventPoster `E.InnerJoin` image) -> do
      E.on (externalEventPoster E.^. ExternalEventPosterImage E.==. image E.^. ImageId)
      E.where_ (externalEventPoster E.^. ExternalEventPosterExternalEvent E.==. E.val externalEventId)
      pure (image E.^. ImageKey)
  pure $ E.unValue <$> listToMaybe keys

getScheduleForParty :: MonadIO m => PartyId -> SqlPersistT m (Maybe (Entity Schedule))
getScheduleForParty partyId = selectOne $
  E.from $ \(partySchedule `E.InnerJoin` schedule) -> do
    E.on (partySchedule E.^. SchedulePartySchedule E.==. schedule E.^. ScheduleId)
    E.where_ (partySchedule E.^. SchedulePartyParty E.==. E.val partyId)
    pure schedule

-- In esqueleto 3.5.1.0, so we can remove it when we get there.
selectOne :: (E.SqlSelect a r, MonadIO m) => E.SqlQuery a -> SqlReadT m (Maybe r)
selectOne q = fmap listToMaybe $ E.select $ E.limit 1 >> q

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
  deleteWhere [SchedulePosterSchedule ==. scheduleId]
  deleteWhere [SchedulePartySchedule ==. scheduleId]
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
    case TE.decodeUtf8' $ LB.toStrict $ JSON.encode v of
      Right t ->
        toWidgetHead $
          H.script ! HA.type_ "application/ld+json" $
            H.text t
      -- Should not happen because JSON.encode spits out utf8 text
      Left _ -> toWidgetHead (mempty :: Html)

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

makeGoogleMapsEmbedUrl :: Text -> Handler (Maybe Text)
makeGoogleMapsEmbedUrl placeQuery = do
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  forM mGoogleAPIKey $ \apiKey ->
    pure $ googleMapsEmbedUrl apiKey placeQuery
