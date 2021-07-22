{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.OptParse where

import Control.Applicative
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
import Looper
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import YamlParse.Applicative as YamlParse

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingPort :: !Int,
    settingLogLevel :: !LogLevel,
    settingDbFile :: !(Path Abs File),
    settingSendEmails :: !Bool,
    settingAdmin :: !(Maybe Text),
    settingEnableOSMGeocoding :: !Bool,
    settingEnableGoogleGeocoding :: !Bool,
    settingGoogleAPIKey :: !(Maybe Text),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text),
    settingSentrySettings :: !(Maybe SentrySettings),
    settingImageGarbageCollectorLooperSettings :: !LooperSettings,
    -- https://events.info
    settingEventsInfoImportLooperSettings :: !LooperSettings,
    -- https://golatindance.com
    settingGolatindanceComImportLooperSettings :: !LooperSettings,
    -- https://danceplace.com
    settingDanceplaceComImportLooperSettings :: !LooperSettings
  }
  deriving (Show, Eq, Generic)

data SentrySettings = SentrySettings
  { sentrySettingDSN :: !Text,
    sentrySettingRelease :: !Text
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
  let settingLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  settingDbFile <- case flagDbFile <|> envDbFile <|> mc confDbFile of
    Nothing -> resolveFile' "salsa-parties.sqlite3"
    Just dbf -> resolveFile' dbf
  let settingSendEmails = fromMaybe False $ flagSendEmails <|> envSendEmails <|> mc confSendEmails
  let settingAdmin = flagAdmin <|> envAdmin <|> mc confAdmin
  let settingEnableOSMGeocoding = fromMaybe True $ flagEnableOSMGeocoding <|> envEnableOSMGeocoding <|> mc confEnableOSMGeocoding
  let settingEnableGoogleGeocoding = fromMaybe True $ flagEnableGoogleGeocoding <|> envEnableGoogleGeocoding <|> mc confEnableGoogleGeocoding
  let settingSentrySettings = combineToSentrySettings flagSentryFlags envSentryEnvironment $ mc confSentryConfiguration
  let settingGoogleAPIKey = flagGoogleAPIKey <|> envGoogleAPIKey <|> mc confGoogleAPIKey
  let settingGoogleAnalyticsTracking = flagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking
  let settingGoogleSearchConsoleVerification = flagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification
  let settingImageGarbageCollectorLooperSettings = deriveLooperSettings (seconds 30) (hours 24) flagImageGarbageCollectorLooperFlags envImageGarbageCollectorLooperEnvironment (mc confImageGarbageCollectorLooperConfiguration)
  let settingEventsInfoImportLooperSettings = deriveLooperSettings (minutes 1) (hours 24) flagEventsInfoImportLooperFlags envEventsInfoImportLooperEnvironment (mc confEventsInfoImportLooperConfiguration)
  let settingGolatindanceComImportLooperSettings = deriveLooperSettings (minutes 2) (hours 24) flagGolatindanceComImportLooperFlags envGolatindanceComImportLooperEnvironment (mc confGolatindanceComImportLooperConfiguration)
  let settingDanceplaceComImportLooperSettings = deriveLooperSettings (minutes 3) (hours 24) flagDanceplaceComImportLooperFlags envDanceplaceComImportLooperEnvironment (mc confDanceplaceComImportLooperConfiguration)
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
  { confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confDbFile :: !(Maybe FilePath),
    confSendEmails :: !(Maybe Bool),
    confAdmin :: !(Maybe Text),
    confEnableOSMGeocoding :: !(Maybe Bool),
    confEnableGoogleGeocoding :: !(Maybe Bool),
    confSentryConfiguration :: !(Maybe SentryConfiguration),
    confGoogleAPIKey :: !(Maybe Text),
    confGoogleAnalyticsTracking :: !(Maybe Text),
    confGoogleSearchConsoleVerification :: !(Maybe Text),
    confImageGarbageCollectorLooperConfiguration :: !(Maybe LooperConfiguration),
    confEventsInfoImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confGolatindanceComImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confDanceplaceComImportLooperConfiguration :: !(Maybe LooperConfiguration)
  }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "port" "Port"
        <*> optionalFieldWith "log-level" "Minimal severity for log messages" viaRead
        <*> optionalField "database" "The path to the database file"
        <*> optionalField "send-emails" "Whether to send emails and require email verification"
        <*> optionalField "admin" "The email address of the admin user"
        <*> optionalField "enable-osm-geocoding" "Enable OpenStreetMaps Geocoding"
        <*> optionalField "enable-google-geocoding" "Enable Google Geocoding"
        <*> optionalField "sentry" "Sentry configuration"
        <*> optionalField "google-api-key" "Google API key"
        <*> optionalField "google-analytics-tracking" "Google analytics tracking code"
        <*> optionalField "google-search-console-verification" "Google search console html element verification code"
        <*> optionalField "image-garbage-collector" "The image garbage collector looper"
        <*> optionalField "events-info-importer" "The events.info import looper"
        <*> optionalField "golatindance-com-importer" "The golatindance.com import looper"
        <*> optionalField "danceplace-com-importer" "The danceplace.com import looper"

data SentryConfiguration = SentryConfiguration
  { sentryConfDSN :: !(Maybe Text),
    sentryConfRelease :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance FromJSON SentryConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema SentryConfiguration where
  yamlSchema =
    objectParser "SentryConfiguration" $
      SentryConfiguration
        <$> optionalField "dsn" "Sentry Data Source Name"
        <*> optionalField "release" "Sentry Release"

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|optparse-template|])
  resolveFile xdgConfigDir "config.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envLogLevel :: !(Maybe LogLevel),
    envDbFile :: !(Maybe FilePath),
    envSendEmails :: !(Maybe Bool),
    envAdmin :: !(Maybe Text),
    envEnableOSMGeocoding :: !(Maybe Bool),
    envEnableGoogleGeocoding :: !(Maybe Bool),
    envSentryEnvironment :: !SentryEnvironment,
    envGoogleAPIKey :: !(Maybe Text),
    envGoogleAnalyticsTracking :: !(Maybe Text),
    envGoogleSearchConsoleVerification :: !(Maybe Text),
    envImageGarbageCollectorLooperEnvironment :: !LooperEnvironment,
    envEventsInfoImportLooperEnvironment :: !LooperEnvironment,
    envGolatindanceComImportLooperEnvironment :: !LooperEnvironment,
    envDanceplaceComImportLooperEnvironment :: !LooperEnvironment
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
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "Port")
      <*> Env.var (fmap Just . Env.auto) "LOG_LEVEL" (mE <> Env.help "Minimal severity for log messages")
      <*> Env.var (fmap Just . Env.auto) "DATABASE" (mE <> Env.help "The path to the database file")
      <*> Env.var (fmap Just . Env.auto) "SEND_EMAILS" (mE <> Env.help "Whether to send emails and require email verification")
      <*> Env.var (fmap Just . Env.str) "ADMIN" (mE <> Env.help "The email address of the admin user")
      <*> Env.var (fmap Just . Env.auto) "ENABLE_OSM_GEOCODING" (mE <> Env.help "Enable OpenStreetMaps Geocoding")
      <*> Env.var (fmap Just . Env.auto) "ENABLE_GOOGLE_GEOCODING" (mE <> Env.help "Enable Google Geocoding")
      <*> sentryEnvironmentParser
      <*> Env.var (fmap Just . Env.str) "GOOGLE_API_KEY" (mE <> Env.help "Google api key")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_ANALYTICS_TRACKING" (mE <> Env.help "Google analytics tracking code")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (mE <> Env.help "Google search console html element verification code")
      <*> looperEnvironmentParser "IMAGE_GARBAGE_COLLECTOR"
      <*> looperEnvironmentParser "EVENTS_INFO_IMPORTER"
      <*> looperEnvironmentParser "GOLATINDANCE_COM_IMPORTER"
      <*> looperEnvironmentParser "DANCEPLACE_COM_IMPORTER"
  where
    mE = Env.def Nothing

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
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagLogLevel :: !(Maybe LogLevel),
    flagDbFile :: !(Maybe FilePath),
    flagSendEmails :: !(Maybe Bool),
    flagAdmin :: !(Maybe Text),
    flagEnableOSMGeocoding :: !(Maybe Bool),
    flagEnableGoogleGeocoding :: !(Maybe Bool),
    flagSentryFlags :: !SentryFlags,
    flagGoogleAPIKey :: !(Maybe Text),
    flagGoogleAnalyticsTracking :: !(Maybe Text),
    flagGoogleSearchConsoleVerification :: !(Maybe Text),
    flagImageGarbageCollectorLooperFlags :: !LooperFlags,
    flagEventsInfoImportLooperFlags :: !LooperFlags,
    flagGolatindanceComImportLooperFlags :: !LooperFlags,
    flagDanceplaceComImportLooperFlags :: !LooperFlags
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
    <*> getLooperFlags "image-garbage-collector"
    <*> getLooperFlags "events-info-importer"
    <*> getLooperFlags "golatindance-com-importer"
    <*> getLooperFlags "danceplace-com-importer"

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
