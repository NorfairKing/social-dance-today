{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

module Salsa.Party.Web.Server.Foundation where

import Control.Concurrent.TokenLimiter
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
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
import Network.HTTP.Client as HTTP
import Path
import Salsa.Party.DB
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Poster
import Salsa.Party.Web.Server.Static
import Salsa.Party.Web.Server.Widget
import System.Random
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Show.Pretty (ppShow)
import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.AutoReload
import Yesod.EmbeddedStatic (EmbeddedStatic)

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appHTTPManager :: !HTTP.Manager,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appSendEmails :: !Bool,
    appAdmin :: !(Maybe Text),
    appOSMRateLimiter :: !(Maybe RateLimiter), -- Nothing means disabled.
    appGoogleAPIKey :: !(Maybe Text), -- Nothing means disabled.
    appGoogleAnalyticsTracking :: !(Maybe Text), -- Nothing means disabled.
    appGoogleSearchConsoleVerification :: !(Maybe Text) -- Nothing means disabled.
  }

mkYesodData "App" $(makeRelativeToProject "routes.txt" >>= parseRoutesFile)

instance Yesod App where
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

  makeSessionBackend a = Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) (fromAbsFile (appSessionKeyFile a))

  shouldLogIO app _ ll = pure $ ll >= appLogLevel app

  maximumContentLengthIO _ route = pure $ case route of
    Just (AccountR AccountSubmitPartyR) -> Nothing -- No limit on the images.
    _ -> Just $ 2 * 1024 * 1024 -- 2 megabytes

  authRoute _ = Just $ AuthR LoginR

  errorHandler NotFound = fmap toTypedContent $
    withNavBar $ do
      setTitle "Page not found"
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

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

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
withFormFailureNavBar errorMessages body = do
  mAuth <- maybeAuth
  mAdmin <- getsYesod appAdmin
  let isAdmin = case mAuth of
        Nothing -> False
        Just (Entity _ user) -> Just (userEmailAddress user) == mAdmin
  defaultLayout $(widgetFile "with-nav-bar")

salsaAuthPlugin :: AuthPlugin App
salsaAuthPlugin = AuthPlugin salsaAuthPluginName dispatch salsaLoginHandler
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR
    dispatch "POST" ["login"] = postLoginR
    dispatch "POST" ["resend-verification-email"] = postResendVerificationEmailR
    dispatch "GET" ["verify", userEmailAddress, verificationKey] = getVerifyR userEmailAddress verificationKey >>= sendResponse
    dispatch _ _ = notFound

salsaAuthPluginName :: Text
salsaAuthPluginName = "salsa"

registerR :: Route Auth
registerR = PluginR salsaAuthPluginName ["register"]

getRegisterR :: AuthHandler App Html
getRegisterR = do
  messages <- getMessages
  token <- genToken
  liftHandler $
    withNavBar $ do
      setTitle "Salsa Parties Today: Registration"
      setDescription "Register an account at Salsa Parties Today"
      $(widgetFile "auth/register")

data RegisterForm = RegisterForm
  { registerFormEmailAddress :: Text,
    registerFormPassphrase :: Password,
    registerFormConfirmPassphrase :: Password
  }
  deriving (Show, Generic)

registerForm :: FormInput Handler RegisterForm
registerForm =
  RegisterForm
    <$> ireq emailField "email-address"
    <*> (mkPassword <$> ireq passwordField "passphrase")
    <*> (mkPassword <$> ireq passwordField "passphrase-confirm")

postRegisterR :: AuthHandler App TypedContent
postRegisterR = liftHandler $ do
  RegisterForm {..} <- runInputPost registerForm
  mUser <- runDB $ getBy (UniqueUserEmailAddress registerFormEmailAddress)
  case mUser of
    Just _ -> do
      setMessage "An account with this username already exists"
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

loginR :: Route Auth
loginR = PluginR salsaAuthPluginName ["login"]

salsaLoginHandler :: (Route Auth -> Route App) -> Widget
salsaLoginHandler _toParentRoute = do
  messages <- getMessages
  token <- genToken
  setTitle "Salsa Parties Today: Login"
  setDescription "Log into your account at Salsa Parties Today"
  $(widgetFile "auth/login")

data LoginForm = LoginForm
  { loginFormEmailAddress :: Text,
    loginFormPassphrase :: Password
  }
  deriving (Show, Generic)

loginForm :: FormInput Handler LoginForm
loginForm =
  LoginForm
    <$> ireq emailField "email-address"
    <*> (mkPassword <$> ireq passwordField "passphrase")

postLoginR :: AuthHandler App TypedContent
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

resendVerificationEmailR :: Route Auth
resendVerificationEmailR = PluginR salsaAuthPluginName ["resend-verification-email"]

postResendVerificationEmailR :: AuthHandler App TypedContent
postResendVerificationEmailR = do
  Entity _ User {..} <- requireAuth
  case userVerificationKey of
    Nothing -> addMessage "" "Account is already verified."
    Just verificationKey -> liftHandler $ sendVerificationEmail userEmailAddress verificationKey
  redirect $ AccountR AccountOverviewR

sendVerificationEmail :: Text -> Text -> Handler ()
sendVerificationEmail userEmailAddress verificationKey = do
  shouldSendEmail <- getsYesod appSendEmails
  if shouldSendEmail
    then do
      logInfoN $ "Sending verification email to address: " <> userEmailAddress

      renderUrl <- getUrlRenderParams

      let subject = SES.content "Email Verification"

      let textBody = SES.content $ LT.toStrict $ LTB.toLazyText $ $(textFile "templates/auth/email/verification-email.txt") renderUrl

      let htmlBody = SES.content $ LT.toStrict $ renderHtml $ $(hamletFile "templates/auth/email/verification-email.hamlet") renderUrl

      let body =
            SES.body
              & SES.bText ?~ textBody
              & SES.bHTML ?~ htmlBody

      let message = SES.message subject body

      let fromEmail = "no-reply@salsa-parties.today"

      let destination =
            SES.destination
              & SES.dBCCAddresses .~ [fromEmail]
              & SES.dToAddresses .~ [userEmailAddress]
      let request = SES.sendEmail fromEmail destination message

      logFunc <- askLoggerIO
      let logger :: AWS.Logger
          logger awsLevel builder =
            let ourLevel = case awsLevel of
                  AWS.Info -> LevelInfo
                  AWS.Error -> LevelError
                  AWS.Debug -> LevelDebug
                  AWS.Trace -> LevelDebug
             in logFunc defaultLoc "aws-client" ourLevel $ toLogStr builder
      awsEnv <- liftIO $ AWS.newEnv AWS.Discover
      let ourAwsEnv =
            awsEnv
              & AWS.envRegion .~ AWS.Ireland
              & AWS.envLogger .~ logger
      response <- AWS.runAWS ourAwsEnv $ AWS.trying AWS._Error $ AWS.send request
      case (^. SES.sersResponseStatus) <$> response of
        Right 200 -> do
          logInfoN $ "Succesfully send verification email to address: " <> userEmailAddress
          addMessageI "is-success" (ConfirmationEmailSent userEmailAddress)
        _ -> do
          logErrorN $ T.unlines ["Failed to send verification email to address: " <> userEmailAddress, T.pack (ppShow response)]
          addMessage "is-danger" "Failed te send verification email."
    else logInfoN $ "Not sending verification email (because sendEmail is turned of), to address: " <> userEmailAddress

verifyR :: Text -> Text -> Route Auth
verifyR userEmailAddress verificationKey = PluginR salsaAuthPluginName ["verify", userEmailAddress, verificationKey]

getVerifyR :: Text -> Text -> AuthHandler App Html
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

prettyDayFormat :: String
prettyDayFormat = "%A, %B %e"

posterImageWidget :: Party -> Organiser -> CASKey -> Widget
posterImageWidget Party {..} Organiser {..} posterKey =
  [whamlet|
    <img
      width=#{desiredWidth}
      height=#{desiredHeight}
      src=@{ImageR posterKey}
      alt="Poster for #{partyTitle} on #{formatTime defaultTimeLocale prettyDayFormat partyDay}, by #{organiserName}">
  |]

getPosterForParty :: MonadIO m => PartyId -> SqlPersistT m (Maybe CASKey)
getPosterForParty partyId = do
  keys <- E.select $
    E.from $ \(partyPoster `E.InnerJoin` image) -> do
      E.on (partyPoster E.^. PartyPosterImage E.==. image E.^. ImageId)
      E.where_ (partyPoster E.^. PartyPosterParty E.==. E.val partyId)
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
  delete organiserId

deletePartyCompletely :: MonadIO m => PartyId -> SqlPersistT m ()
deletePartyCompletely partyId = do
  deleteWhere [PartyPosterParty ==. partyId]
  delete partyId

appDB :: (MonadReader App m, MonadLoggerIO m) => SqlPersistT (LoggingT IO) a -> m a
appDB func = do
  pool <- asks appConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool func pool) logFunc
