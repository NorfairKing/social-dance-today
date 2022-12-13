{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- We use a lot of type-class constraints here just to be able to put it in a separate module
module Salsa.Party.Web.Server.Foundation.Auth
  ( module Salsa.Party.Web.Server.Foundation.Auth,
    module Salsa.Party.Web.Server.Foundation.Auth.Routes,
  )
where

import qualified Amazonka.SES as SES
import qualified Amazonka.SES.SendEmail as SES
import qualified Amazonka.SES.Types as SES
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Data.Validity.Text ()
import Data.Validity.Time ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import Salsa.Party.DB
import Salsa.Party.Email
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.Auth.Routes
import Salsa.Party.Web.Server.Foundation.I18N.Messages
import Salsa.Party.Web.Server.Foundation.NavBar
import Salsa.Party.Web.Server.Foundation.Yesod.Data
import Salsa.Party.Web.Server.Widget
import System.Random
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Yesod
import Yesod.Auth
import Yesod.Auth.Message

{-# ANN salsaAuthPlugin ("NOCOVER" :: String) #-}
salsaAuthPlugin ::
  forall app.
  ( app ~ App,
    YesodAuth app,
    AuthId app ~ UserId,
    YesodPersist app,
    YesodAuthPersist app,
    AuthEntity app ~ User,
    BaseBackend (YesodPersistBackend app) ~ SqlBackend,
    PersistUniqueRead (YesodPersistBackend app),
    PersistStoreWrite (YesodPersistBackend app)
  ) =>
  AuthPlugin app
salsaAuthPlugin = AuthPlugin salsaAuthPluginName dispatch salsaLoginHandler
  where
    dispatch :: Text -> [Text] -> AuthHandler app TypedContent
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR
    dispatch "POST" ["login"] = postLoginR
    dispatch "POST" ["resend-verification-email"] = postResendVerificationEmailR
    dispatch "GET" ["verify", userEmailAddress, verificationKey] = getVerifyR (EmailAddress userEmailAddress) verificationKey >>= sendResponse
    dispatch _ _ = notFound

getRegisterR ::
  ( app ~ App,
    YesodAuth app,
    AuthId app ~ UserId,
    YesodAuthPersist app,
    AuthEntity app ~ User
  ) =>
  AuthHandler app Html
getRegisterR = do
  messages <- getMessages
  token <- genToken
  liftHandler $
    withNavBar $ do
      setTitleI MsgRegistrationTitle
      setDescriptionIdempI MsgRegistrationDescription
      $(widgetFile "auth/register")

data RegisterForm = RegisterForm
  { registerFormEmailAddress :: !EmailAddress,
    registerFormPassphrase :: !Password,
    registerFormConfirmPassphrase :: !Password
  }
  deriving (Show, Generic)

registerForm ::
  ( MonadHandler m,
    RenderMessage (HandlerSite m) FormMessage
  ) =>
  FormInput m RegisterForm
registerForm =
  RegisterForm
    <$> (EmailAddress <$> ireq emailField "email-address")
    <*> (mkPassword <$> ireq passwordField "passphrase")
    <*> (mkPassword <$> ireq passwordField "passphrase-confirm")

postRegisterR ::
  ( app ~ App,
    YesodAuth app,
    YesodPersist app,
    BaseBackend (YesodPersistBackend app) ~ SqlBackend,
    PersistUniqueRead (YesodPersistBackend app),
    PersistStoreWrite (YesodPersistBackend app)
  ) =>
  AuthHandler app TypedContent
postRegisterR = liftHandler $ do
  RegisterForm {..} <- runInputPost registerForm
  mUser <- runDB $ getBy (UniqueUserEmailAddress registerFormEmailAddress)
  case mUser of
    Just _ -> do
      setMessageI MsgRegistrationErrorAccountAlreadyExists
      redirect $ AuthR registerR
    Nothing -> do
      if unsafeShowPassword registerFormPassphrase == unsafeShowPassword registerFormConfirmPassphrase
        then do
          verificationKey <- liftIO $ T.pack <$> replicateM 32 (randomRIO ('a', 'z'))
          hashDifficulty <- getsYesod appHashDifficulty
          passphraseHash <- liftIO $ hashPasswordWithParams hashDifficulty registerFormPassphrase
          now <- liftIO getCurrentTime
          runDB $
            insert_
              User
                { userEmailAddress = registerFormEmailAddress,
                  userPassphraseHash = passphraseHash,
                  userVerificationKey = Just verificationKey,
                  userCreated = now
                }
          sendVerificationEmail registerFormEmailAddress verificationKey
          setCredsRedirect
            Creds
              { credsPlugin = salsaAuthPluginName,
                credsIdent = emailAddressText registerFormEmailAddress,
                credsExtra = []
              }
        else do
          addMessageI "is-danger" PassMismatch
          redirect $ AuthR registerR

salsaLoginHandler ::
  (app ~ App) =>
  (Route Auth -> Route app) ->
  WidgetFor app ()
salsaLoginHandler _toParentRoute = do
  messages <- getMessages
  token <- genToken
  setTitleI MsgLoginTitle
  setDescriptionIdempI MsgLoginDescription
  $(widgetFile "auth/login")

data LoginForm = LoginForm
  { loginFormEmailAddress :: !EmailAddress,
    loginFormPassphrase :: !Password
  }
  deriving (Show, Generic)

loginForm ::
  ( MonadHandler m,
    RenderMessage (HandlerSite m) FormMessage
  ) =>
  FormInput m LoginForm
loginForm =
  LoginForm
    <$> (EmailAddress <$> ireq emailField "email-address")
    <*> (mkPassword <$> ireq passwordField "passphrase")

{-# ANN postLoginR ("NOCOVER" :: String) #-}
postLoginR ::
  ( app ~ App,
    YesodAuth app,
    YesodPersist app,
    BaseBackend (YesodPersistBackend app) ~ SqlBackend,
    PersistUniqueRead (YesodPersistBackend app)
  ) =>
  AuthHandler app TypedContent
postLoginR = do
  LoginForm {..} <- liftHandler $ runInputPost loginForm
  mUser <- liftHandler $ runDB $ getBy (UniqueUserEmailAddress loginFormEmailAddress)
  let loginFail = loginErrorMessageI LoginR InvalidLogin
  case mUser of
    Nothing -> loginFail
    Just (Entity _ User {..}) ->
      case checkPassword loginFormPassphrase userPassphraseHash of
        PasswordCheckSuccess ->
          setCredsRedirect
            Creds
              { credsPlugin = salsaAuthPluginName,
                credsIdent = emailAddressText loginFormEmailAddress,
                credsExtra = []
              }
        PasswordCheckFail -> loginFail

postResendVerificationEmailR ::
  ( app ~ App,
    YesodAuth app,
    AuthId app ~ UserId,
    YesodAuthPersist app,
    AuthEntity app ~ User,
    RenderMessage app AppMessage,
    RedirectUrl app (Route App)
  ) =>
  AuthHandler app TypedContent
postResendVerificationEmailR = do
  Entity _ User {..} <- requireAuth
  case userVerificationKey of
    Nothing -> addMessageI "" MsgVerificationErrorAlreadyVerified
    Just verificationKey -> liftHandler $ sendVerificationEmail userEmailAddress verificationKey
  redirect $ AccountR AccountOverviewR

sendVerificationEmail ::
  ( app ~ App,
    YesodAuth app,
    RenderMessage app AppMessage
  ) =>
  EmailAddress ->
  Text ->
  HandlerFor app ()
sendVerificationEmail userEmailAddress verificationKey = do
  logInfoN $ T.pack $ unwords ["Sending verification email to address:", show userEmailAddress]

  urlRender <- getUrlRenderParams
  messageRender <- getMessageRender

  let subject = SES.newContent $ messageRender $ MsgVerificationEmailSubject siteTitle

  let textBody = SES.newContent $ LT.toStrict $ LTB.toLazyText $ $(textFile "templates/auth/email/verification-email.txt") urlRender

  let htmlBody = SES.newContent $ LT.toStrict $ renderHtml $ $(ihamletFile "templates/auth/email/verification-email.hamlet") (toHtml . messageRender) urlRender

  let body = SES.newBody {SES.html = Just htmlBody, SES.text = Just textBody}

  let message = SES.newMessage subject body

  let destination = SES.newDestination {SES.toAddresses = Just [emailAddressText userEmailAddress]}

  app <- getYesod

  case appSendAddress app of
    Nothing -> pure ()
    Just sendAddress -> do
      let request =
            (SES.newSendEmail sendAddress destination message)
              { SES.replyToAddresses = Just $ maybeToList (emailAddressText <$> appAdmin app)
              }
      sendEmailResult <- sendEmail app request
      case sendEmailResult of
        ErrorWhileSendingEmail _ -> do
          addMessageI "is-danger" MsgVerificationEmailFailure
          logErrorN $ T.pack $ unwords ["Failed to send verification email to address:", show userEmailAddress]
        EmailSentSuccesfully -> do
          addMessageI "is-success" (ConfirmationEmailSent $ emailAddressText userEmailAddress)
          logInfoN $ T.pack $ unwords ["Succesfully send verification email to address:", show userEmailAddress]
        NoEmailSent -> pure ()

getVerifyR ::
  ( app ~ App,
    YesodAuth app,
    YesodPersist app,
    BaseBackend (YesodPersistBackend app) ~ SqlBackend,
    PersistUniqueRead (YesodPersistBackend app),
    PersistStoreWrite (YesodPersistBackend app)
  ) =>
  EmailAddress ->
  Text ->
  AuthHandler app Html
getVerifyR emailAddress verificationKey = liftHandler $ do
  runDB $ do
    mUser <- getBy (UniqueUserEmailAddress emailAddress)
    -- If the user is unverified, they'll have a verification key which has to match the given key.
    -- If anything goes wrong, we must not send back notFound because then we leak account existence.
    case mUser of
      Nothing -> pure ()
      Just (Entity userId User {..}) ->
        when (userVerificationKey == Just verificationKey) $ do
          addMessageI "is-success" EmailVerified
          update userId [UserVerificationKey =. Nothing]
  redirect $ AccountR AccountOverviewR

requireAdmin ::
  ( app ~ App,
    AuthId app ~ UserId,
    AuthEntity app ~ User,
    YesodAuthPersist app
  ) =>
  HandlerFor app ()
requireAdmin = do
  Entity _ user <- requireAuth
  mAdmin <- getsYesod appAdmin
  case mAdmin of
    Nothing -> notFound -- No one can access it if there is no admin.
    Just adminEmailAddress ->
      if adminEmailAddress == userEmailAddress user
        then pure ()
        else notFound
