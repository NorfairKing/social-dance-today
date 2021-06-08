{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Foundation where

import Control.Monad
import Data.Text (Text)
import Path
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Static
import Salsa.Party.Web.Server.Widget
import Text.Hamlet
import Yesod
import Yesod.AutoReload
import Yesod.EmbeddedStatic

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app
  defaultLayout widget = do
    app <- getYesod
    messages <- getMessages
    let withAutoReload =
          if development
            then (<> autoReloadWidgetFor ReloadR)
            else id
    let body = withAutoReload $(widgetFile "default-body")
    pageContent <- widgetToPageContent body
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")
  makeSessionBackend a = Just <$> defaultClientSessionBackend 120 (fromAbsFile (appSessionKeyFile a))

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool func pool

getReloadR :: Handler ()
getReloadR = getAutoReloadR

genToken :: MonadHandler m => m Html
genToken = do
  alreadyExpired
  req <- getRequest
  let tokenKey = defaultCsrfParamName
  pure $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

withMFormResultNavBar :: Maybe (FormResult a) -> Widget -> Handler Html
withMFormResultNavBar = maybe withNavBar withFormResultNavBar

withFormResultNavBar :: FormResult a -> Widget -> Handler Html
withFormResultNavBar fr w =
  case fr of
    FormSuccess _ -> withNavBar w
    FormFailure ts -> withFormFailureNavBar ts w
    FormMissing -> withFormFailureNavBar ["Missing data"] w

withNavBar :: Widget -> Handler Html
withNavBar = withFormFailureNavBar []

withFormFailureNavBar :: [Text] -> Widget -> Handler Html
withFormFailureNavBar msgs body = do
  let mmsg =
        Just $
          forM_ msgs $ \msg ->
            [shamlet|<div role="alert">#{msg}
          |]
  defaultLayout $(widgetFile "with-nav-bar")
