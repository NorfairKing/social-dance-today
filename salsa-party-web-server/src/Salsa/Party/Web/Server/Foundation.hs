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

module Salsa.Party.Web.Server.Foundation
  ( module Salsa.Party.Web.Server.Foundation,
    module Salsa.Party.Web.Server.Foundation.I18N,
    module Salsa.Party.Web.Server.Foundation.Yesod,
    module Salsa.Party.Web.Server.Foundation.App,
  )
where

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
import Salsa.Party.Web.Server.Foundation.I18N
import Salsa.Party.Web.Server.Foundation.Yesod
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

getReloadR :: Handler ()
getReloadR = getAutoReloadR

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
      setTitleI MsgRegistrationTitle
      setDescriptionI MsgRegistrationDescription
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

loginR :: Route Auth
loginR = PluginR salsaAuthPluginName ["login"]

salsaLoginHandler :: (Route Auth -> Route App) -> Widget
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
    Nothing -> addMessageI "" MsgVerificationErrorAlreadyVerified
    Just verificationKey -> liftHandler $ sendVerificationEmail userEmailAddress verificationKey
  redirect $ AccountR AccountOverviewR

sendVerificationEmail :: Text -> Text -> Handler ()
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

runAWS :: (MonadUnliftIO m, MonadLoggerIO m) => AWS.AWS a -> m (Either AWS.Error a)
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

newtype JSONLDData = JSONLDData Value

toJSONLDData :: ToJSON a => a -> JSONLDData
toJSONLDData = JSONLDData . toJSON

instance ToWidgetHead App JSONLDData where
  toWidgetHead (JSONLDData v) =
    toWidgetHead $
      H.script ! HA.type_ "application/ld+json" $
        H.lazyText $ renderJavascript $ toJavascript v
