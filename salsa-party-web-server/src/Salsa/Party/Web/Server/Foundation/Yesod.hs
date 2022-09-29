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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Foundation.Yesod
  ( module Salsa.Party.Web.Server.Foundation.Yesod,
    module Salsa.Party.Web.Server.Foundation.Yesod.Data,
  )
where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Path
import Salsa.Party.DB
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.Auth
import Salsa.Party.Web.Server.Foundation.I18N.Messages
import Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage
import Salsa.Party.Web.Server.Foundation.NavBar
import Salsa.Party.Web.Server.Foundation.Yesod.Data
import Salsa.Party.Web.Server.Static
import Salsa.Party.Web.Server.Widget
import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.AutoReload

instance Yesod App where
  approot = ApprootMaster $ fromMaybe "" . appRoot
  defaultLayout widget = do
    app <- getYesod
    messages <- getMessages
    currentLang <- supportedLanguageAbbreviation <$> getFirstMatchingSupportedLanguage
    let withAutoReload =
          if development
            then (<> autoReloadWidgetFor ReloadR)
            else id

    mCurrentRoute <- getCurrentRoute

    req <- getRequest
    let params = filter ((/= languageQueryParameter) . fst) (reqGetParams req)

    mCanonicalRoute <- forM mCurrentRoute $ \currentRoute -> do
      toTextUrl (currentRoute, params)

    languageRoutes <- case mCurrentRoute of
      Nothing -> pure []
      Just currentRoute -> forM supportedLanguages $ \lang -> do
        url <- toTextUrl (currentRoute, (languageQueryParameter, supportedLanguageAbbreviation lang) : params)
        pure (lang, url)

    let withSentry =
          case appSentrySettings app of
            Nothing -> id
            Just sentrySettings -> case mCurrentRoute of
              Just (AdminR _) -> id
              _ -> (<> sentryWidget sentrySettings)

    let body = withSentry $
          withAutoReload $ do
            addScript $ StaticR set_utcoffset_js
            $(widgetFile "default-body")

    pageContent <- widgetToPageContent body
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

  makeSessionBackend a =
    sslOnlySessions -- Secure
      . strictSameSiteSessions -- SameSite=strict
      $ Just
        <$> defaultClientSessionBackend
          (60 * 24 * 365 * 10)
          (fromAbsFile (appSessionKeyFile a))

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
        mAuth <- maybeAuth
        case mAuth of
          Nothing -> notFound
          Just (Entity _ u) -> do
            mAdmin <- getsYesod appAdmin
            if Just (userEmailAddress u) == mAdmin
              then pure Authorized
              else notFound
      _ -> pure Authorized

  yesodMiddleware =
    defaultYesodMiddleware
      . setLanguageMiddleware
      -- https://infosec.mozilla.org/guidelines/web_security#http-strict-transport-security
      -- Strict-Transport-Security
      . sslOnlyMiddleware (2 * 365 * 24 * 60) -- Two years in minutes
      . setSecurityHeaders

setLanguageMiddleware :: Handler a -> Handler a
setLanguageMiddleware handler = do
  mLParam <- lookupGetParam languageQueryParameter
  forM_ mLParam $ \l -> setLanguage l
  handler

setSecurityHeaders :: Handler a -> Handler a
setSecurityHeaders handler = do
  -- https://infosec.mozilla.org/guidelines/web_security#content-security-policy
  -- TODO: unsafe inline
  -- addHeader
  --   "Content-Security-Policy"
  --   "default-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline'"

  -- https://infosec.mozilla.org/guidelines/web_security#x-content-type-options
  addHeader "X-Content-Type-Options" "nosniff"

  -- https://infosec.mozilla.org/guidelines/web_security#x-frame-options
  addHeader "X-Frame-Options" "DENY"
  handler

languageQueryParameter :: Text
languageQueryParameter = "l"

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = AccountR AccountOverviewR
  logoutDest _ = HomeR
  authenticate Creds {..} =
    let byEmail = do
          mUser <- liftHandler $ runDB $ getBy (UniqueUserEmailAddress (EmailAddress credsIdent))
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

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

sentryWidget :: SentrySettings -> Widget
sentryWidget SentrySettings {..} = do
  addScript $ StaticR sentry_js
  $(widgetFile "sentry")

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool (retryOnBusy func) pool
