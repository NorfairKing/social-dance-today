{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Foundation
  ( module Salsa.Party.Web.Server.Foundation,
    module Salsa.Party.Web.Server.Foundation.NavBar,
    module Salsa.Party.Web.Server.Foundation.Auth,
    module Salsa.Party.Web.Server.Widget,
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
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity.Text ()
import Data.Validity.Time ()
import qualified Database.Esqueleto.Legacy as E
import Database.Persist.Sql
import Database.Persist.Sqlite
import qualified ICal
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.Auth
import Salsa.Party.Web.Server.Foundation.I18N
import Salsa.Party.Web.Server.Foundation.NavBar
import Salsa.Party.Web.Server.Foundation.Yesod
import Salsa.Party.Web.Server.Widget
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod

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

getScheduleForParty :: MonadIO m => PartyId -> SqlPersistT m (Maybe (Entity Schedule))
getScheduleForParty partyId = E.selectOne $
  E.from $ \(partySchedule `E.InnerJoin` schedule) -> do
    E.on (partySchedule E.^. SchedulePartySchedule E.==. schedule E.^. ScheduleId)
    E.where_ (partySchedule E.^. SchedulePartyParty E.==. E.val partyId)
    pure schedule

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
deletePartyCompletely = delete

deleteScheduleCompletely :: MonadIO m => ScheduleId -> SqlPersistT m ()
deleteScheduleCompletely scheduleId = do
  deleteWhere [SchedulePartySchedule ==. scheduleId]
  delete scheduleId

appDB :: (MonadReader App m, MonadLoggerIO m) => SqlPersistT (LoggingT IO) a -> m a
appDB func = do
  pool <- asks appConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool (retryOnBusy func) pool) logFunc

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

instance HasContentType ICal.Calendar where
  getContentType _ = typeCalendar

instance ToContent ICal.Calendar where
  toContent = toContent . ICal.renderICalendarByteString . (: [])

instance ToTypedContent ICal.Calendar where
  toTypedContent vCalendar = TypedContent typeCalendar $ toContent vCalendar

typeCalendar :: ContentType
typeCalendar = "text/calendar"

organiserRoute :: Organiser -> Route App
organiserRoute organiser = fromMaybe (OrganiserR (organiserUuid organiser)) $ organiserSlugRoute organiser

organiserCalendarRoute :: Organiser -> Route App
organiserCalendarRoute organiser = fromMaybe (OrganiserCalendarR (organiserUuid organiser)) $ organiserSlugCalendarRoute organiser

organiserSlugRoute :: Organiser -> Maybe (Route App)
organiserSlugRoute Organiser {..} = do
  organiserSlug_ <- organiserSlug
  pure $ OrganiserSlugR organiserSlug_

organiserSlugCalendarRoute :: Organiser -> Maybe (Route App)
organiserSlugCalendarRoute Organiser {..} = do
  organiserSlug_ <- organiserSlug
  pure $ OrganiserSlugCalendarR organiserSlug_

makeOrganiserSlug :: Text -> Maybe OrganiserSlug
makeOrganiserSlug = mkSlug

partyRoute :: Organiser -> Party -> Route App
partyRoute organiser party = fromMaybe (EventR (partyUuid party)) $ partySlugRoute organiser party

partySlugRoute :: Organiser -> Party -> Maybe (Route App)
partySlugRoute Organiser {..} Party {..} = do
  organiserSlug_ <- organiserSlug
  partySlug_ <- partySlug
  pure $ PartySlugR organiserSlug_ partySlug_ partyDay

makePartySlug :: Text -> Maybe EventSlug
makePartySlug = mkSlug

externalEventRoute :: ExternalEvent -> Route App
externalEventRoute externalEvent = fromMaybe (EventR (externalEventUuid externalEvent)) $ externalEventSlugRoute externalEvent

externalEventSlugRoute :: ExternalEvent -> Maybe (Route App)
externalEventSlugRoute ExternalEvent {..} = do
  externalEventSlug_ <- externalEventSlug
  pure $ ExternalEventSlugR externalEventSlug_ externalEventDay

-- We add two random letters to the end of the external event so that it's more
-- likely to that the slug will be unique even if we find the same exact event
-- in two places.
makeExternalEventSlug :: EventUUID -> Text -> Maybe EventSlug
makeExternalEventSlug uuid title = mkSlug $ T.pack $ T.unpack title <> [replacementChar] <> take 2 (uuidString uuid)

locateMeButton :: RenderMessage App AppMessage => Text -> Text -> Text -> WidgetFor App ()
locateMeButton queryId statusId helpId = do
  messageRender <- getMessageRender
  $(widgetFile "locate-button")

partyTitleMessage :: Party -> AppMessage
partyTitleMessage party =
  if partyCancelled party
    then MsgPartyTitleCancelled (partyTitle party)
    else MsgPartyTitleScheduled (partyTitle party)

externalEventTitleMessage :: ExternalEvent -> AppMessage
externalEventTitleMessage externalEvent = case externalEventCancelled externalEvent of
  Just True -> MsgPartyTitleCancelled (externalEventTitle externalEvent)
  _ -> MsgPartyTitleScheduled (externalEventTitle externalEvent)

organiserNameField :: forall m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text
organiserNameField =
  checkM
    ( pure
        . (Right :: Text -> Either FormMessage Text)
        . normaliseOrganiserName
    )
    textField

titleField :: forall m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text
titleField =
  checkM
    ( pure
        . (Right :: Text -> Either FormMessage Text)
        . normaliseTitle
    )
    textField

descriptionField :: forall m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Textarea
descriptionField =
  checkM
    ( pure . (Right :: Textarea -> Either FormMessage Textarea)
        . normaliseDescriptionTextarea
    )
    textareaField
