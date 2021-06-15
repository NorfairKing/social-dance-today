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

import Control.Monad
import Data.Fixed
import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Path
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Static
import Salsa.Party.Web.Server.Widget
import System.Random
import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.AutoReload
import Yesod.Core.Types
import Yesod.EmbeddedStatic (EmbeddedStatic)

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appHTTPManager :: !HTTP.Manager,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appSendEmails :: !Bool,
    appAdmin :: !(Maybe Text),
    appGoogleAPIKey :: !(Maybe Text),
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
  authRoute _ = Just $ AuthR LoginR
  isAuthorized route _ =
    -- List each route that a user can access without login
    -- so we don't accidentally authorize anything.
    case route of
      HomeR -> pure Authorized
      QueryR -> pure Authorized
      SearchR _ -> pure Authorized
      PartyR _ -> pure Authorized
      PosterR _ -> pure Authorized
      OrganiserR _ -> pure Authorized
      ReloadR -> pure Authorized
      FaviconR -> pure Authorized
      AuthR _ -> pure Authorized
      StaticR _ -> pure Authorized
      AdminR _ -> do
        -- Has to be admin
        mAuthId <- maybeAuth
        case mAuthId of
          Nothing -> pure AuthenticationRequired
          Just (Entity _ u) -> do
            mAdmin <- getsYesod appAdmin
            pure $
              if Just (userEmailAddress u) == mAdmin
                then Authorized
                else AuthenticationRequired
      _ -> do
        -- Has to be logged-in
        mAuthId <- maybeAuthId
        case mAuthId of
          Nothing -> pure AuthenticationRequired
          Just _ -> pure Authorized

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool func pool

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = AccountR
  logoutDest _ = HomeR
  authenticate Creds {..} = case credsPlugin of
    "salsa" -> do
      mUser <- liftHandler $ runDB $ getBy (UniqueUserEmailAddress credsIdent)
      pure $ case mUser of
        Nothing -> UserError $ IdentifierNotFound credsIdent
        Just (Entity userId _) -> Authenticated userId
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
  currentRoute <- getCurrentRoute
  mAuthId <- maybeAuthId
  defaultLayout $(widgetFile "with-nav-bar")

salsaAuthPlugin :: AuthPlugin App
salsaAuthPlugin = AuthPlugin salsaAuthPluginName dispatch salsaLoginHandler
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR
    dispatch "POST" ["login"] = postLoginR
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
          -- TODO send email here.
          -- addMessageI "is-success" ConfirmationEmailSentTitle
          passphraseHash <- liftIO $ hashPassword registerFormPassphrase
          void $
            runDB $
              insertBy
                ( User
                    { userEmailAddress = registerFormEmailAddress,
                      userPassphraseHash = passphraseHash,
                      userVerificationKey = Just verificationKey
                    }
                )
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

handleFaviconR :: Handler TypedContent
handleFaviconR = redirect $ StaticR favicon_ico

data Coordinates = Coordinates
  { coordinatesLat :: !Nano,
    coordinatesLon :: !Nano
  }
  deriving (Show, Eq, Generic)

instance Validity Coordinates

instance Validity Textarea where
  validate = validate . unTextarea

instance Validity FileInfo where
  validate = trivialValidation

instance Show FileInfo where
  show fileInfo = show ("File with name: " <> fileName fileInfo)

instance Eq FileInfo where
  (==) = (==) `on` fileName

deriving instance Generic FileInfo

-- This could potentially be dangerous if a type is read than written
instance HasResolution a => PathPiece (Fixed a) where
  fromPathPiece = fmap MkFixed . fromPathPiece
  toPathPiece (MkFixed i) = toPathPiece i

placeCoordinates :: Place -> Coordinates
placeCoordinates Place {..} = Coordinates {coordinatesLat = placeLat, coordinatesLon = placeLon}

prettyDayFormat :: String
prettyDayFormat = "%A, %B %e"
