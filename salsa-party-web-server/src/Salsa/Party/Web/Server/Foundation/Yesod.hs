{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Salsa.Party.Web.Server.Foundation.Yesod where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Retry
import Data.FileEmbed
import Data.Fixed
import Data.Function
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import GHC.Generics (Generic)
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Network.HTTP.Client.Retry
import Path
import Salsa.Party.DB
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.Auth
import Salsa.Party.Web.Server.Foundation.I18N.Messages
import Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage
import Salsa.Party.Web.Server.Foundation.Yesod.Routes
import Salsa.Party.Web.Server.Poster
import Salsa.Party.Web.Server.Static
import Salsa.Party.Web.Server.Widget
import System.Random
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Hamlet
import Text.Julius
import Text.Shakespeare.Text
import Text.Show.Pretty (ppShow)
import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.AutoReload
import Yesod.EmbeddedStatic (EmbeddedStatic)

mkMessage "App" "messages" "en"

mkYesodData "App" $(makeRelativeToProject "routes.txt" >>= parseRoutesFile)

instance Yesod App where
  approot = guessApprootOr $ ApprootMaster $ fromMaybe "" . appRoot
  defaultLayout widget = do
    app <- getYesod
    messages <- getMessages
    lang <- supportedLanguageAbbreviation <$> getFirstMatchingSupportedLanguage
    let withAutoReload =
          if development
            then (<> autoReloadWidgetFor ReloadR)
            else id
    currentRoute <- getCurrentRoute
    let withSentry =
          case appSentrySettings app of
            Nothing -> id
            Just sentrySettings -> case currentRoute of
              Just (AdminR _) -> id
              _ -> (<> sentryWidget sentrySettings)
    let body = withSentry $ withAutoReload $(widgetFile "default-body")
    pageContent <- widgetToPageContent body
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

  makeSessionBackend a = Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) (fromAbsFile (appSessionKeyFile a))

  shouldLogIO app _ ll = pure $ ll >= appLogLevel app

  maximumContentLengthIO _ route = pure $ case route of
    Just (AccountR AccountSubmitPartyR) -> Nothing -- No limit on the images.
    _ -> Just $ 2 * 1024 * 1024 -- 2 megabytes

  authRoute _ = Just $ AuthR LoginR

  errorHandler NotFound = fmap toTypedContent $
    withNavBar $ do
      setTitleI MsgNotFound
      $(widgetFile "error/404")
  errorHandler route = defaultErrorHandler route

  isAuthorized route _ =
    -- List each route that a user can access without login
    -- so we don't accidentally authorize anything.
    case route of
      AccountR _ -> do
        -- Has to be logged-in
        mAuthId <- maybeAuthId
        case mAuthId of
          Nothing -> pure AuthenticationRequired
          Just _ -> pure Authorized
      AdminR _ -> do
        -- Has to be admin
        mAuthId <- maybeAuth
        case mAuthId of
          Nothing -> notFound
          Just (Entity _ u) -> do
            mAdmin <- getsYesod appAdmin
            if Just (userEmailAddress u) == mAdmin
              then pure Authorized
              else notFound
      _ -> pure Authorized

sentryWidget :: SentrySettings -> Widget
sentryWidget SentrySettings {..} = do
  addScript $ StaticR sentry_js
  $(widgetFile "sentry")

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool func pool

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = AccountR AccountOverviewR
  logoutDest _ = HomeR
  authenticate Creds {..} =
    let byEmail = do
          mUser <- liftHandler $ runDB $ getBy (UniqueUserEmailAddress credsIdent)
          pure $ case mUser of
            Nothing -> UserError $ IdentifierNotFound credsIdent
            Just (Entity userId _) -> Authenticated userId
     in case credsPlugin of
          "impersonation" -> byEmail
          "salsa" -> byEmail
          _ -> pure $ ServerError "Unknown auth plugin"
  onLogin = addMessageI "is-success" NowLoggedIn
  authPlugins _ = [salsaAuthPlugin]

instance YesodAuthPersist App

type PageNumber = Int

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
withFormFailureNavBar errorMessages body = do
  mAuth <- maybeAuth
  mAdmin <- getsYesod appAdmin
  let isAdmin = case mAuth of
        Nothing -> False
        Just (Entity _ user) -> Just (userEmailAddress user) == mAdmin
  lang <- getFirstMatchingSupportedLanguage
  defaultLayout $(widgetFile "with-nav-bar")
