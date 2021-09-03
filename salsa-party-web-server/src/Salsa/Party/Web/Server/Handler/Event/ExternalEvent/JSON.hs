{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
  ( externalEventPageJSON,
    ExternalEventExport (..),
    externalEventJSONExport,
    importExternalEventJSONExport,
  )
where

import Data.Aeson as JSON
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.URI
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar as ICal
import Yesod
import Yesod.Core.Types

externalEventPageJSON :: Entity ExternalEvent -> Handler (JSONResponse ExternalEventExport)
externalEventPageJSON (Entity _ externalEvent) = do
  place@Place {..} <- runDB $ get404 $ externalEventPlace externalEvent
  renderUrl <- getUrlRender
  pure $ JSONResponse $ externalEventJSONExport renderUrl externalEvent place

externalEventJSONExport :: (Route App -> Text) -> ExternalEvent -> Place -> ExternalEventExport
externalEventJSONExport renderUrl externalEvent@ExternalEvent {..} place =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
   in ExternalEventExport

data ExternalEventExport = ExternalEventExport
  deriving (Show, Eq, Generic)

instance Validity ExternalEventExport

instance ToJSON ExternalEventExport where
  toJSON ExternalEventExport = object []

instance FromJSON ExternalEventExport where
  parseJSON = withObject "ExternalEventExport" $ \o ->
    pure ExternalEventExport

importExternalEventJSONExport :: MonadIO m => ExternalEventExport -> SqlPersistT m ExternalEventId
importExternalEventJSONExport ExternalEventExport = undefined
