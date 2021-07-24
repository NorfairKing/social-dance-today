{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-fields #-}

module Salsa.Party.Web.Server.Foundation where

import Control.Applicative
import Control.Concurrent.TokenLimiter.Concurrent
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
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Constants
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

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appHTTPManager :: !HTTP.Manager,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appSendEmails :: !Bool,
    appAdmin :: !(Maybe Text),
    appOSMRateLimiter :: !(Maybe TokenLimiter), -- Nothing means disabled.
    appSentrySettings :: !(Maybe SentrySettings), -- Nothing means disabled.
    appGoogleAPIKey :: !(Maybe Text), -- Nothing means disabled.
    appGoogleAnalyticsTracking :: !(Maybe Text), -- Nothing means disabled.
    appGoogleSearchConsoleVerification :: !(Maybe Text) -- Nothing means disabled.
  }

mkMessage "App" "messages" "en"

mkYesodData "App" $(makeRelativeToProject "routes.txt" >>= parseRoutesFile)

instance Yesod App where
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
          addMessageI "is-danger" MsgVerificationEmailFailure
    else logInfoN $ "Not sending verification email (because sendEmail is turned of), to address: " <> userEmailAddress

sendAdminNotification :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Text -> m ()
sendAdminNotification notificationContents = do
  shouldSendEmail <- asks appSendEmails
  mAdminEmailAddress <- asks appAdmin
  forM_ mAdminEmailAddress $ \adminEmailAddress ->
    if shouldSendEmail
      then do
        logInfoN $ "Sending Admin Notification email to address: " <> adminEmailAddress

        let subject = SES.content "Admin Notification"

        let textBody = SES.content $ LT.toStrict $(stextFile "templates/email/admin-notification.txt")

        let htmlBody = SES.content $ LT.toStrict $ renderHtml $(shamletFile "templates/email/admin-notification.hamlet")

        let body =
              SES.body
                & SES.bText ?~ textBody
                & SES.bHTML ?~ htmlBody

        let message = SES.message subject body

        let fromEmail = "no-reply@salsa-parties.today"

        let destination =
              SES.destination
                & SES.dBCCAddresses .~ [fromEmail]
                & SES.dToAddresses .~ [adminEmailAddress]
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
        response <- AWS.runResourceT $ AWS.runAWS ourAwsEnv $ AWS.trying AWS._Error $ AWS.send request
        case (^. SES.sersResponseStatus) <$> response of
          Right 200 -> logInfoN $ "Succesfully send verification email to address: " <> adminEmailAddress
          _ -> logErrorN $ T.unlines ["Failed to send verification email to address: " <> adminEmailAddress, T.pack (ppShow response)]
      else logInfoN $ "Not sending admin notification email (because sendEmail is turned of), to address: " <> adminEmailAddress

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

posterImageWidget :: Party -> Organiser -> CASKey -> Widget
posterImageWidget Party {..} Organiser {..} posterKey = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  let timeStr = formatTime timeLocale prettyDayFormat partyDay
  [whamlet|
    <img .poster
      width=#{desiredWidth}
      height=#{desiredHeight}
      src=@{ImageR posterKey}
      alt=_{MsgPosterAltFull partyTitle timeStr organiserName}>
  |]

externalEventPosterImageWidget :: ExternalEvent -> CASKey -> Widget
externalEventPosterImageWidget ExternalEvent {..} posterKey = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  let timeStr = formatTime timeLocale prettyDayFormat externalEventDay
  let altMsg = case externalEventOrganiser of
        Nothing -> MsgPosterAltTitle externalEventTitle timeStr
        Just organiserName -> MsgPosterAltFull externalEventTitle timeStr organiserName
  [whamlet|
      <img .poster
        width=#{desiredWidth}
        height=#{desiredHeight}
        src=@{ImageR posterKey}
        alt=_{altMsg}>
  |]
    <> posterCSS

posterCSS :: Widget
posterCSS =
  toWidget
    [lucius|
  .poster {
    max-width: #{show desiredWidth}px;
    max-height: #{show desiredHeight}px;
    width: 100%;
    height: 100%;
    object-fit: scale-down;
    object-position:left;
  }
  |]

getPosterForParty :: MonadIO m => PartyId -> SqlPersistT m (Maybe CASKey)
getPosterForParty partyId = do
  keys <- E.select $
    E.from $ \(partyPoster `E.InnerJoin` image) -> do
      E.on (partyPoster E.^. PartyPosterImage E.==. image E.^. ImageId)
      E.where_ (partyPoster E.^. PartyPosterParty E.==. E.val partyId)
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

newtype JSONLDData = JSONLDData Value

toJSONLDData :: ToJSON a => a -> JSONLDData
toJSONLDData = JSONLDData . toJSON

instance ToWidgetHead App JSONLDData where
  toWidgetHead (JSONLDData v) =
    toWidgetHead $
      H.script ! HA.type_ "application/ld+json" $
        H.lazyText $ renderJavascript $ toJavascript v

type PageNumber = Int

-- I18N Languages
-- TODO see if we can refactor this out
-- Then also get rid of the partiel field ignoring option in OPTIONS_GHC above
data SupportedLanguage
  = SupportedLangEnglish
  | SupportedLangGerman
  | SupportedLangDutch
  deriving (Show, Read, Eq, Enum, Bounded)

instance PathPiece SupportedLanguage where
  fromPathPiece = parseSupportedLanguage
  toPathPiece = supportedLanguageAbbreviation

supportedLanguages :: [SupportedLanguage]
supportedLanguages = [minBound .. maxBound]

parseSupportedLanguage :: Text -> Maybe SupportedLanguage
parseSupportedLanguage =
  \case
    "en" -> Just SupportedLangEnglish
    "de" -> Just SupportedLangGerman
    "nl" -> Just SupportedLangDutch
    _ -> Nothing

supportedLanguageAbbreviation :: SupportedLanguage -> Text
supportedLanguageAbbreviation =
  \case
    SupportedLangEnglish -> "en"
    SupportedLangGerman -> "de"
    SupportedLangDutch -> "nl"

supportedLanguageNative :: SupportedLanguage -> Text
supportedLanguageNative =
  \case
    SupportedLangEnglish -> "English"
    SupportedLangGerman -> "Deutsch"
    SupportedLangDutch -> "Nederlands"

supportedLanguageEnglish :: SupportedLanguage -> Text
supportedLanguageEnglish =
  \case
    SupportedLangEnglish -> "English"
    SupportedLangGerman -> "German"
    SupportedLangDutch -> "Dutch"

getFirstMatchingSupportedLanguage :: MonadHandler m => m SupportedLanguage
getFirstMatchingSupportedLanguage = do
  ls <- languages
  pure $ fromMaybe SupportedLangEnglish $ firstMatchingSupportedLanguage ls

firstMatchingSupportedLanguage :: [Text] -> Maybe SupportedLanguage
firstMatchingSupportedLanguage =
  \case
    [] -> Nothing
    (l : ls) -> parseSupportedLanguage l <|> firstMatchingSupportedLanguage ls

postSelectLanguageR :: SupportedLanguage -> Handler Html
postSelectLanguageR lang = do
  setLanguage $ supportedLanguageAbbreviation lang
  setUltDestReferer
  redirectUltDest HomeR

getTimeLocale :: MonadHandler m => m TimeLocale
getTimeLocale = languageTimeLocale <$> getFirstMatchingSupportedLanguage

languageTimeLocale :: SupportedLanguage -> TimeLocale
languageTimeLocale = \case
  SupportedLangEnglish -> defaultTimeLocale -- The default in the 'time' package is american.
  SupportedLangGerman -> germanTimeLocale
  SupportedLangDutch -> dutchTimeLocale

getPrettyDayFormat :: MonadHandler m => m String
getPrettyDayFormat = languagePrettyDayFormat <$> getFirstMatchingSupportedLanguage

languagePrettyDayFormat :: SupportedLanguage -> String
languagePrettyDayFormat = \case
  SupportedLangEnglish -> "%A, %B %e" -- Friday, July 16
  SupportedLangGerman -> "%A, %e %B" -- Freitag, 16 juli
  SupportedLangDutch -> "%A, %e %B" -- Vrijdag, 16 juli

getPrettyDateTimeFormat :: MonadHandler m => m String
getPrettyDateTimeFormat = languagePrettyDateTimeFormat <$> getFirstMatchingSupportedLanguage

languagePrettyDateTimeFormat :: SupportedLanguage -> String
languagePrettyDateTimeFormat = \case
  SupportedLangEnglish -> "%A, %B %e - %H:%M" -- Friday, July 16 - 18:30
  SupportedLangGerman -> "%A, %e %B - %H:%M" -- Freitag, 16 juli - 18:30
  SupportedLangDutch -> "%A, %e %B - %H:%M" -- Vrijdag, 16 juli - 18:30

-- | Locale representing German usage.
germanTimeLocale :: TimeLocale
germanTimeLocale =
  TimeLocale
    { wDays =
        [ ("Sonntag", "So"),
          ("Montag", "Mo"),
          ("Dienstag", "Di"),
          ("Mittwoch", "Mi"),
          ("Donnerstag", "Do"),
          ("Freitag", "Fr"),
          ("Samstag", "Sa")
        ],
      months =
        [ ("januar", "Jan."),
          ("februar", "Feb."),
          ("märz", "März."),
          ("april", "Apr."),
          ("mai", "Mai."),
          ("juni", "Juni."),
          ("juli", "Juli."),
          ("august", "Aug."),
          ("september", "Sept."),
          ("oktober", "Okt."),
          ("november", "Nov."),
          ("dezember", "Dez.")
        ],
      amPm = ("AM", "PM"), -- Not used.
      dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
      dateFmt = "%d.%m.%y",
      timeFmt = "%H:%M:%S",
      time12Fmt = "%I:%M:%S %p", -- Not used.
      knownTimeZones = [] -- Don't need it.
    }

-- | Locale representing Dutch usage.
dutchTimeLocale :: TimeLocale
dutchTimeLocale =
  TimeLocale
    { wDays =
        [ ("zondag", "zo."),
          ("maandag", "ma."),
          ("dinsdag", "di."),
          ("woensdag", "wo."),
          ("donderdag", "do."),
          ("vrijdag", "vr."),
          ("zaterdag", "za.")
        ],
      months =
        [ ("januari", "jan."),
          ("februari", "feb."),
          ("maart", "mrt."),
          ("april", "apr."),
          ("mei", "mei"),
          ("juni", "jun."),
          ("juli", "jul."),
          ("augustus", "aug."),
          ("september", "sept."),
          ("oktober", "okt."),
          ("november", "nov."),
          ("december", "dec.")
        ],
      amPm = ("AM", "PM"), -- Not used.
      dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
      dateFmt = "%d-%m-%y",
      timeFmt = "%H:%M:%S",
      time12Fmt = "%I:%M:%S %p", -- Not used.
      knownTimeZones = [] -- Don't need it.
    }

-- TODO test this function
autoDayMsg :: Day -> Day -> AppMessage
autoDayMsg today day =
  let d = diffDays day today
   in case compare d 0 of
        EQ -> MsgDayToday
        LT -> case d of
          -1 -> MsgDayYesterday
          -2 -> MsgDay2DaysAgo
          -3 -> MsgDay3DaysAgo
          -4 -> MsgDay4DaysAgo
          -5 -> MsgDay5DaysAgo
          -6 -> MsgDay6DaysAgo
          i
            | i > (- 2 * 7) -> MsgDay1WeekAgo
            | i > (- 3 * 7) -> MsgDay2WeeksAgo
            | i > (- 4 * 7) -> MsgDay3WeeksAgo
            | i > (- 1 * 30) -> MsgDay4WeeksAgo
            | i > (- 2 * 30) -> MsgDay1MonthAgo
            | i > (- 3 * 30) -> MsgDay2MonthsAgo
            | i > (- 4 * 30) -> MsgDay3MonthsAgo
            | i > (- 5 * 30) -> MsgDay4MonthsAgo
            | i > (- 6 * 30) -> MsgDay5MonthsAgo
            | i > (- 7 * 30) -> MsgDay6MonthsAgo
            | i > (- 8 * 30) -> MsgDay7MonthsAgo
            | i > (- 9 * 30) -> MsgDay8MonthsAgo
            | i > (- 10 * 30) -> MsgDay9MonthsAgo
            | i > (- 11 * 30) -> MsgDay10MonthsAgo
            | i > -365 -> MsgDay11MonthsAgo
            | otherwise -> MsgDayMoreThanAYearAgo
        GT -> case d of
          1 -> MsgDayTomorrow
          2 -> MsgDayIn2Days
          3 -> MsgDayIn3Days
          4 -> MsgDayIn4Days
          5 -> MsgDayIn5Days
          6 -> MsgDayIn6Days
          i
            | i < 2 * 7 -> MsgDayIn1Week
            | i < 3 * 7 -> MsgDayIn2Week
            | i < 4 * 7 -> MsgDayIn3Week
            | i < 1 * 30 -> MsgDayIn4Week
            | i < 2 * 30 -> MsgDayIn1Month
            | i < 3 * 30 -> MsgDayIn2Months
            | i < 4 * 30 -> MsgDayIn3Months
            | i < 5 * 30 -> MsgDayIn4Months
            | i < 6 * 30 -> MsgDayIn5Months
            | i < 7 * 30 -> MsgDayIn6Months
            | i < 8 * 30 -> MsgDayIn7Months
            | i < 9 * 30 -> MsgDayIn8Months
            | i < 10 * 30 -> MsgDayIn9Months
            | i < 11 * 30 -> MsgDayIn10Months
            | i < 365 -> MsgDayIn11Months
            | otherwise -> MsgDayInMoreThanAYear
