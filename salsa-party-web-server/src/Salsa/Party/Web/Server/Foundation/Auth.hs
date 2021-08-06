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

-- We use a lot of type-class constraints here just to be able to put it in a separate module
module Salsa.Party.Web.Server.Foundation.Auth
  ( module Salsa.Party.Web.Server.Foundation.Auth,
    module Salsa.Party.Web.Server.Foundation.Auth.Routes,
  )
where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Retry
import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Data.Validity.Text ()
import Data.Validity.Time ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Network.HTTP.Client.Retry
import Salsa.Party.DB
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
import Text.Show.Pretty (ppShow)
import Yesod
import Yesod.Auth
import Yesod.Auth.Message

salsaAuthPlugin ::
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
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR
    dispatch "POST" ["login"] = postLoginR
    dispatch "POST" ["resend-verification-email"] = postResendVerificationEmailR
    dispatch "GET" ["verify", userEmailAddress, verificationKey] = getVerifyR userEmailAddress verificationKey >>= sendResponse
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
      setDescriptionI MsgRegistrationDescription
      $(widgetFile "auth/register")

data RegisterForm = RegisterForm
  { registerFormEmailAddress :: Text,
    registerFormPassphrase :: Password,
    registerFormConfirmPassphrase :: Password
  }
  deriving (Show, Generic)

registerForm ::
  ( MonadHandler m,
    RenderMessage (HandlerSite m) FormMessage
  ) =>
  FormInput m RegisterForm
registerForm =
  RegisterForm
    <$> ireq emailField "email-address"
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
          passphraseHash <- liftIO $ hashPassword registerFormPassphrase
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
          setCredsRedirect Creds {credsPlugin = salsaAuthPluginName, credsIdent = registerFormEmailAddress, credsExtra = []}
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
  setDescriptionI MsgLoginDescription
  $(widgetFile "auth/login")

data LoginForm = LoginForm
  { loginFormEmailAddress :: Text,
    loginFormPassphrase :: Password
  }
  deriving (Show, Generic)

loginForm ::
  ( MonadHandler m,
    RenderMessage (HandlerSite m) FormMessage
  ) =>
  FormInput m LoginForm
loginForm =
  LoginForm
    <$> ireq emailField "email-address"
    <*> (mkPassword <$> ireq passwordField "passphrase")

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
        PasswordCheckSuccess -> setCredsRedirect Creds {credsPlugin = salsaAuthPluginName, credsIdent = loginFormEmailAddress, credsExtra = []}
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
  Text ->
  Text ->
  HandlerFor app ()
sendVerificationEmail userEmailAddress verificationKey = do
  shouldSendEmail <- getsYesod appSendEmails
  mSendAddress <- getsYesod appSendAddress
  forM_ mSendAddress $ \sendAddress ->
    if shouldSendEmail
      then do
        logInfoN $ "Sending verification email to address: " <> userEmailAddress

        urlRender <- getUrlRenderParams
        messageRender <- getMessageRender

        let subject = SES.content $ messageRender MsgVerificationEmailSubject

        let textBody = SES.content $ LT.toStrict $ LTB.toLazyText $ $(textFile "templates/auth/email/verification-email.txt") urlRender

        let htmlBody = SES.content $ LT.toStrict $ renderHtml $ $(ihamletFile "templates/auth/email/verification-email.hamlet") (toHtml . messageRender) urlRender

        let body =
              SES.body
                & SES.bText ?~ textBody
                & SES.bHTML ?~ htmlBody

        let message = SES.message subject body

        let destination =
              SES.destination
                & SES.dToAddresses .~ [userEmailAddress]
        let request = SES.sendEmail sendAddress destination message

        response <- runAWS $ AWS.send request
        case (^. SES.sersResponseStatus) <$> response of
          Right 200 -> do
            logInfoN $ "Succesfully send verification email to address: " <> userEmailAddress
            addMessageI "is-success" (ConfirmationEmailSent userEmailAddress)
          _ -> do
            logErrorN $ T.unlines ["Failed to send verification email to address: " <> userEmailAddress, T.pack (ppShow response)]
            addMessageI "is-danger" MsgVerificationEmailFailure
      else logInfoN $ "Not sending verification email (because sendEmail is turned of), to address: " <> userEmailAddress

runAWS ::
  ( MonadUnliftIO m,
    MonadLoggerIO m
  ) =>
  AWS.AWS a ->
  m (Either AWS.Error a)
runAWS func = do
  logger <- mkAwsLogger
  awsEnv <- liftIO $ AWS.newEnv AWS.Discover
  let ourAwsEnv =
        awsEnv
          & AWS.envRegion .~ AWS.Ireland
          & AWS.envLogger .~ logger

  let awsRetryPolicy = httpRetryPolicy
  let shouldRetry = \case
        Left awsError -> case awsError of
          AWS.TransportError exception -> shouldRetryHttpException exception
          AWS.SerializeError _ -> pure False
          AWS.ServiceError se -> pure $ shouldRetryStatusCode $ AWS._serviceStatus se
        Right _ -> pure False -- Didn't even fail.
  let tryOnce = AWS.runResourceT $ AWS.runAWS ourAwsEnv $ AWS.trying AWS._Error func

  retrying awsRetryPolicy (\_ -> shouldRetry) (\_ -> tryOnce)

mkAwsLogger :: MonadLoggerIO m => m AWS.Logger
mkAwsLogger = do
  logFunc <- askLoggerIO
  let logger awsLevel builder =
        let ourLevel = case awsLevel of
              AWS.Info -> LevelInfo
              AWS.Error -> LevelError
              AWS.Debug -> LevelDebug
              AWS.Trace -> LevelDebug
         in logFunc defaultLoc "aws-client" ourLevel $ toLogStr builder
  pure logger

getVerifyR ::
  ( app ~ App,
    YesodAuth app,
    YesodPersist app,
    BaseBackend (YesodPersistBackend app) ~ SqlBackend,
    PersistUniqueRead (YesodPersistBackend app),
    PersistStoreWrite (YesodPersistBackend app)
  ) =>
  Text ->
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
