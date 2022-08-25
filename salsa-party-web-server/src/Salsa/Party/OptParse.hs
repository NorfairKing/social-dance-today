{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Monad
import Control.Monad.Logger
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Env
import GHC.Generics (Generic)
import Looper
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import Salsa.Party.DB

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingHost :: !(Maybe Text),
    settingPort :: !Int,
    settingLogLevel :: !LogLevel,
    settingDbFile :: !(Path Abs File),
    settingSendEmails :: !Bool,
    settingSendAddress :: !(Maybe Text),
    settingAdmin :: !(Maybe EmailAddress),
    settingEnableOSMGeocoding :: !Bool,
    settingEnableGoogleGeocoding :: !Bool,
    settingGoogleAPIKey :: !(Maybe Text),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text),
    settingSentrySettings :: !(Maybe SentrySettings),
    settingSearchCachePopulatorLooperSettings :: !LooperSettings,
    settingExploreCachePopulatorLooperSettings :: !LooperSettings,
    settingOrganiserReminderLooperSettings :: !LooperSettings,
    settingPartyGarbageCollectorLooperSettings :: !LooperSettings,
    settingImageGarbageCollectorLooperSettings :: !LooperSettings,
    settingPartySchedulerLooperSettings :: !LooperSettings,
    settingImporterInterval :: !NominalDiffTime,
    settingImporterSettings :: !(Map Text LooperSettings)
  }
  deriving (Show, Eq, Generic)

data SentrySettings = SentrySettings
  { sentrySettingDSN :: !Text,
    sentrySettingRelease :: !Text
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let settingHost = ("https://" <>) <$> (flagHost <|> envHost <|> mc confHost)
  let settingPort = fromMaybe 8080 $ flagPort <|> envPort <|> mc confPort
  let settingLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  settingDbFile <- case flagDbFile <|> envDbFile <|> mc confDbFile of
    Nothing -> resolveFile' "salsa-parties.sqlite3"
    Just dbf -> resolveFile' dbf
  let settingSendEmails = fromMaybe False $ flagSendEmails <|> envSendEmails <|> mc confSendEmails
  let settingSendAddress = flagSendAddress <|> envSendAddress <|> mc confSendAddress
  let settingAdmin = flagAdmin <|> envAdmin <|> mc confAdmin
  let settingEnableOSMGeocoding = fromMaybe True $ flagEnableOSMGeocoding <|> envEnableOSMGeocoding <|> mc confEnableOSMGeocoding
  let settingEnableGoogleGeocoding = fromMaybe True $ flagEnableGoogleGeocoding <|> envEnableGoogleGeocoding <|> mc confEnableGoogleGeocoding
  let settingSentrySettings = combineToSentrySettings flagSentryFlags envSentryEnvironment $ mc confSentryConfiguration
  let settingGoogleAPIKey = flagGoogleAPIKey <|> envGoogleAPIKey <|> mc confGoogleAPIKey
  let settingGoogleAnalyticsTracking = flagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking
  let settingGoogleSearchConsoleVerification = flagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification
  let settingSearchCachePopulatorLooperSettings = deriveLooperSettings (seconds 15) (hours 1) flagSearchCachePopulatorLooperFlags envSearchCachePopulatorLooperEnvironment (mc confSearchCachePopulatorLooperConfiguration)
  let settingExploreCachePopulatorLooperSettings = deriveLooperSettings (seconds 15) (hours 6) flagExploreCachePopulatorLooperFlags envExploreCachePopulatorLooperEnvironment (mc confExploreCachePopulatorLooperConfiguration)
  let settingOrganiserReminderLooperSettings = deriveLooperSettings (seconds 30) (hours 24) flagOrganiserReminderLooperFlags envOrganiserReminderLooperEnvironment (mc confOrganiserReminderLooperConfiguration)
  let settingPartyGarbageCollectorLooperSettings = deriveLooperSettings (minutes 1) (hours 24) flagPartyGarbageCollectorLooperFlags envPartyGarbageCollectorLooperEnvironment (mc confPartyGarbageCollectorLooperConfiguration)
  let settingImageGarbageCollectorLooperSettings = deriveLooperSettings (minutes 1 + seconds 30) (hours 24) flagImageGarbageCollectorLooperFlags envImageGarbageCollectorLooperEnvironment (mc confImageGarbageCollectorLooperConfiguration)
  let settingPartySchedulerLooperSettings = deriveLooperSettings (minutes 2 + seconds 30) (hours 24) flagPartySchedulerLooperFlags envPartySchedulerLooperEnvironment (mc confPartySchedulerLooperConfiguration)

  let settingImporterInterval = maybe (hours 24) fromIntegral $ flagImporterInterval <|> envImporterInterval <|> mc confImporterInterval
  settingImporterSettings <- fmap M.fromList $
    forM (zip [0 ..] dataSources) $ \(ix, dataSource) -> do
      flags <- case M.lookup dataSource flagImporterFlags of
        Nothing -> fail $ unwords ["No flags for data source", show dataSource]
        Just fs -> pure fs
      env <- case M.lookup dataSource envImporterEnvironments of
        Nothing -> fail $ unwords ["No environment for data source", show dataSource]
        Just e -> pure e
      let mLConf = M.lookup dataSource (maybe M.empty confImporterConfigurations mConf)
      pure (dataSource, deriveLooperSettings (minutes 3 + hours ix) (hours 24) flags env mLConf)

  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

combineToSentrySettings :: SentryFlags -> SentryEnvironment -> Maybe SentryConfiguration -> Maybe SentrySettings
combineToSentrySettings SentryFlags {..} SentryEnvironment {..} mc =
  SentrySettings
    <$> (sentryFlagDSN <|> sentryEnvDSN <|> (mc >>= sentryConfDSN))
    <*> (sentryFlagRelease <|> sentryEnvRelease <|> (mc >>= sentryConfRelease))

data Configuration = Configuration
  { confHost :: !(Maybe Text),
    confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confDbFile :: !(Maybe FilePath),
    confSendEmails :: !(Maybe Bool),
    confSendAddress :: !(Maybe Text),
    confAdmin :: !(Maybe EmailAddress),
    confEnableOSMGeocoding :: !(Maybe Bool),
    confEnableGoogleGeocoding :: !(Maybe Bool),
    confSentryConfiguration :: !(Maybe SentryConfiguration),
    confGoogleAPIKey :: !(Maybe Text),
    confGoogleAnalyticsTracking :: !(Maybe Text),
    confGoogleSearchConsoleVerification :: !(Maybe Text),
    confSearchCachePopulatorLooperConfiguration :: !(Maybe LooperConfiguration),
    confExploreCachePopulatorLooperConfiguration :: !(Maybe LooperConfiguration),
    confOrganiserReminderLooperConfiguration :: !(Maybe LooperConfiguration),
    confPartyGarbageCollectorLooperConfiguration :: !(Maybe LooperConfiguration),
    confImageGarbageCollectorLooperConfiguration :: !(Maybe LooperConfiguration),
    confPartySchedulerLooperConfiguration :: !(Maybe LooperConfiguration),
    confImporterInterval :: !(Maybe Int),
    confImporterConfigurations :: !(Map Text LooperConfiguration)
  }
  deriving (Show, Eq, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "host" "The host, example: salsa-party.cs-syd.eu" .= confHost
        <*> optionalFieldOrNull "port" "Port" .= confPort
        <*> optionalFieldOrNull "log-level" "Minimal severity for log messages" .= confLogLevel
        <*> optionalFieldOrNull "database" "The path to the database file" .= confDbFile
        <*> optionalFieldOrNull "send-emails" "Whether to send emails and require email verification" .= confSendEmails
        <*> optionalFieldOrNull "send-address" "The email address to send emails from" .= confSendAddress
        <*> optionalFieldOrNull "admin" "The email address of the admin user" .= confAdmin
        <*> optionalFieldOrNull "enable-osm-geocoding" "Enable OpenStreetMaps Geocoding" .= confEnableOSMGeocoding
        <*> optionalFieldOrNull "enable-google-geocoding" "Enable Google Geocoding" .= confEnableGoogleGeocoding
        <*> optionalFieldOrNull "sentry" "Sentry configuration" .= confSentryConfiguration
        <*> optionalFieldOrNull "google-api-key" "Google API key" .= confGoogleAPIKey
        <*> optionalFieldOrNull "google-analytics-tracking" "Google analytics tracking code" .= confGoogleAnalyticsTracking
        <*> optionalFieldOrNull "google-search-console-verification" "Google search console html element verification code" .= confGoogleSearchConsoleVerification
        <*> optionalFieldOrNull "search-cache-populator" "The search cache populator looper" .= confSearchCachePopulatorLooperConfiguration
        <*> optionalFieldOrNull "explore-cache-populator" "The explore cache populator looper" .= confExploreCachePopulatorLooperConfiguration
        <*> optionalFieldOrNull "organiser-reminder" "The organiser reminder looper" .= confOrganiserReminderLooperConfiguration
        <*> optionalFieldOrNull "party-garbage-collector" "The party garbage collector looper" .= confPartyGarbageCollectorLooperConfiguration
        <*> optionalFieldOrNull "image-garbage-collector" "The image garbage collector looper" .= confImageGarbageCollectorLooperConfiguration
        <*> optionalFieldOrNull "party-scheduler" "The party scheduler looper" .= confPartySchedulerLooperConfiguration
        <*> optionalFieldOrNull "importer-interval" "The default interval for importers" .= confImporterInterval
        <*> optionalFieldOrNullWithOmittedDefault "importer" M.empty "The per-importer configurations" .= confImporterConfigurations

data SentryConfiguration = SentryConfiguration
  { sentryConfDSN :: !(Maybe Text),
    sentryConfRelease :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance HasCodec SentryConfiguration where
  codec =
    object "SentryConfiguration" $
      SentryConfiguration
        <$> optionalField "dsn" "Sentry Data Source Name" .= sentryConfDSN
        <*> optionalField "release" "Sentry Release" .= sentryConfRelease

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' "config.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envHost :: !(Maybe Text),
    envPort :: !(Maybe Int),
    envLogLevel :: !(Maybe LogLevel),
    envDbFile :: !(Maybe FilePath),
    envSendEmails :: !(Maybe Bool),
    envSendAddress :: !(Maybe Text),
    envAdmin :: !(Maybe EmailAddress),
    envEnableOSMGeocoding :: !(Maybe Bool),
    envEnableGoogleGeocoding :: !(Maybe Bool),
    envSentryEnvironment :: !SentryEnvironment,
    envGoogleAPIKey :: !(Maybe Text),
    envGoogleAnalyticsTracking :: !(Maybe Text),
    envGoogleSearchConsoleVerification :: !(Maybe Text),
    envSearchCachePopulatorLooperEnvironment :: !LooperEnvironment,
    envExploreCachePopulatorLooperEnvironment :: !LooperEnvironment,
    envOrganiserReminderLooperEnvironment :: !LooperEnvironment,
    envPartyGarbageCollectorLooperEnvironment :: !LooperEnvironment,
    envImageGarbageCollectorLooperEnvironment :: !LooperEnvironment,
    envPartySchedulerLooperEnvironment :: !LooperEnvironment,
    envImporterInterval :: !(Maybe Int),
    envImporterEnvironments :: !(Map Text LooperEnvironment)
  }
  deriving (Show, Eq, Generic)

data SentryEnvironment = SentryEnvironment
  { sentryEnvDSN :: !(Maybe Text),
    sentryEnvRelease :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SALSA_PARTY_WEB_SERVER_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.str) "HOST" (mE <> Env.help "Host, example: salsa-party.cs-syd.eu")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "Port")
      <*> Env.var (fmap Just . logLevelReader) "LOG_LEVEL" (mE <> Env.help "Minimal severity for log messages")
      <*> Env.var (fmap Just . Env.auto) "DATABASE" (mE <> Env.help "The path to the database file")
      <*> Env.var (fmap Just . Env.auto) "SEND_EMAILS" (mE <> Env.help "Whether to send emails and require email verification")
      <*> Env.var (fmap Just . Env.str) "SEND_ADDRESS" (mE <> Env.help "The address to send emails from")
      <*> Env.var (fmap Just . Env.str) "ADMIN" (mE <> Env.help "The email address of the admin user")
      <*> Env.var (fmap Just . Env.auto) "ENABLE_OSM_GEOCODING" (mE <> Env.help "Enable OpenStreetMaps Geocoding")
      <*> Env.var (fmap Just . Env.auto) "ENABLE_GOOGLE_GEOCODING" (mE <> Env.help "Enable Google Geocoding")
      <*> sentryEnvironmentParser
      <*> Env.var (fmap Just . Env.str) "GOOGLE_API_KEY" (mE <> Env.help "Google API key")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_ANALYTICS_TRACKING" (mE <> Env.help "Google analytics tracking code")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (mE <> Env.help "Google search console html element verification code")
      <*> looperEnvironmentParser "SEARCH_CACHE_POPULATOR"
      <*> looperEnvironmentParser "EXPLORE_CACHE_POPULATOR"
      <*> looperEnvironmentParser "ORGANISER_REMINDER"
      <*> looperEnvironmentParser "PARTY_GARBAGE_COLLECTOR"
      <*> looperEnvironmentParser "IMAGE_GARBAGE_COLLECTOR"
      <*> looperEnvironmentParser "PARTY_SCHEDULER"
      <*> Env.var (fmap Just . Env.auto) "IMPORTER_INTERVAL" (mE <> Env.help "The default interval for the importers")
      <*> importerEnvironments
  where
    logLevelReader = \case
      "Debug" -> Right LevelDebug
      "Info" -> Right LevelInfo
      "Warn" -> Right LevelWarn
      "Error" -> Right LevelError
      s -> Left $ Env.UnreadError $ "Unknown log level: " <> s
    mE = Env.def Nothing

importerEnvironments :: Env.Parser Env.Error (Map Text LooperEnvironment)
importerEnvironments =
  M.fromList
    <$> traverse
      (\ds -> (,) ds <$> looperEnvironmentParser (dataSourceEnvVar ds))
      dataSources

dataSourceEnvVar :: Text -> String
dataSourceEnvVar = (<> "_IMPORTER") . T.unpack . T.toUpper . T.replace "-" "_" . T.replace "." "_"

sentryEnvironmentParser :: Env.Parser Env.Error SentryEnvironment
sentryEnvironmentParser =
  Env.prefixed "SENTRY_" $
    SentryEnvironment
      <$> Env.var (fmap Just . Env.str) "DSN" (mE <> Env.help "Sentry Data Source Name")
      <*> Env.var (fmap Just . Env.str) "RELEASE" (mE <> Env.help "Sentry Release")
  where
    mE = Env.def Nothing

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagHost :: !(Maybe Text),
    flagPort :: !(Maybe Int),
    flagLogLevel :: !(Maybe LogLevel),
    flagDbFile :: !(Maybe FilePath),
    flagSendEmails :: !(Maybe Bool),
    flagSendAddress :: !(Maybe Text),
    flagAdmin :: !(Maybe EmailAddress),
    flagEnableOSMGeocoding :: !(Maybe Bool),
    flagEnableGoogleGeocoding :: !(Maybe Bool),
    flagSentryFlags :: !SentryFlags,
    flagGoogleAPIKey :: !(Maybe Text),
    flagGoogleAnalyticsTracking :: !(Maybe Text),
    flagGoogleSearchConsoleVerification :: !(Maybe Text),
    flagSearchCachePopulatorLooperFlags :: !LooperFlags,
    flagExploreCachePopulatorLooperFlags :: !LooperFlags,
    flagOrganiserReminderLooperFlags :: !LooperFlags,
    flagPartyGarbageCollectorLooperFlags :: !LooperFlags,
    flagImageGarbageCollectorLooperFlags :: !LooperFlags,
    flagPartySchedulerLooperFlags :: !LooperFlags,
    flagImporterInterval :: !(Maybe Int),
    flagImporterFlags :: !(Map Text LooperFlags)
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          str
          ( mconcat
              [ long "host",
                help "Host, example: salsa-party.cs-syd.eu",
                metavar "HOST"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                help "Port",
                metavar "PORT"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                help "Minimal severity for log messages",
                metavar "LOG_LEVEL"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "database",
                help "The path to the database file",
                metavar "LOG_LEVEL"
              ]
          )
      )
    <*> optional
      ( flag'
          True
          ( mconcat
              [ long "send-emails",
                help "Send emails and require email verification"
              ]
          )
          <|> flag'
            False
            ( mconcat
                [ long "nosend-emails",
                  help "Don't send emails or require email verification"
                ]
            )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "send-address",
                help "The email address to send emails from"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "admin",
                help "Email address of the admin user"
              ]
          )
      )
    <*> optional
      ( flag'
          True
          ( mconcat
              [ long "enable-osm-geocoding",
                help "Enable OpenStreetMaps Geocoding"
              ]
          )
          <|> flag'
            False
            ( mconcat
                [ long "disable-osm-geocoding",
                  help "Disable OpenStreetMaps Geocoding"
                ]
            )
      )
    <*> optional
      ( flag'
          True
          ( mconcat
              [ long "enable-google-geocoding",
                help "Enable Google Geocoding"
              ]
          )
          <|> flag'
            False
            ( mconcat
                [ long "disable-google-geocoding",
                  help "Disable Google Geocoding"
                ]
            )
      )
    <*> parseSentryFlags
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-api-key",
                help "Google API key",
                metavar "API_KEY"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-analytics-tracking",
                help "Google analytics tracking code",
                metavar "CODE"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-search-console-verification",
                help "Google search console html element verification code",
                metavar "CODE"
              ]
          )
      )
    <*> getLooperFlags "search-cache-populator"
    <*> getLooperFlags "explore-cache-populator"
    <*> getLooperFlags "organiser-reminder"
    <*> getLooperFlags "party-garbage-collector"
    <*> getLooperFlags "image-garbage-collector"
    <*> getLooperFlags "party-scheduler"
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "importer-interval",
                help "The default interval for importers",
                metavar "SECONDS"
              ]
          )
      )
    <*> getImporterFlags

getImporterFlags :: OptParse.Parser (Map Text LooperFlags)
getImporterFlags =
  M.fromList
    <$> traverse
      (\ds -> (,) ds <$> getLooperFlags (dataSourceFlagPrefix ds))
      dataSources

dataSourceFlagPrefix :: Text -> String
dataSourceFlagPrefix = (<> "-importer") . T.unpack . T.toLower . T.replace "." "-"

data SentryFlags = SentryFlags
  { sentryFlagDSN :: !(Maybe Text),
    sentryFlagRelease :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

parseSentryFlags :: OptParse.Parser SentryFlags
parseSentryFlags =
  SentryFlags
    <$> optional
      ( strOption
          ( mconcat
              [ long "sentry-dsn",
                help "Sentry Data Source Name",
                metavar "DSN"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "sentry-release",
                help "Sentry Release",
                metavar "Release"
              ]
          )
      )

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

dataSources :: [Text]
dataSources =
  [ "salsa-berlin.de",
    "tanzevents.ch",
    "danceus.org",
    "events.info",
    "golatindance.com",
    "danceplace.com",
    "mapdance.com",
    "salsachicago.com",
    "dancefloorfinder.com",
    "sensual.dance",
    "salsa.be",
    "latinworld.nl",
    "tanzagenda.ch",
    "stayhappening.com",
    "londonsalsaevents.com",
    "salsalovers.be"
  ]
