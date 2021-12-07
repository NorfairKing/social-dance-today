{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
    settingOrganiserReminderLooperSettings :: !LooperSettings,
    settingImageGarbageCollectorLooperSettings :: !LooperSettings,
    settingPartySchedulerLooperSettings :: !LooperSettings,
    settingImporterInterval :: NominalDiffTime,
    -- https://events.info
    settingEventsInfoImportLooperSettings :: !LooperSettings,
    -- https://golatindance.com
    settingGolatindanceComImportLooperSettings :: !LooperSettings,
    -- https://danceplace.com
    settingDanceplaceComImportLooperSettings :: !LooperSettings,
    -- https://mapdance.com
    settingMapdanceComImportLooperSettings :: !LooperSettings,
    -- https://salsachicago.com
    settingSalsachicagoComImportLooperSettings :: !LooperSettings,
    -- https://dancefloorfinder.com
    settingDancefloorfinderComImportLooperSettings :: !LooperSettings,
    -- https://sensual.dance
    settingSensualDanceImportLooperSettings :: !LooperSettings,
    -- https://salsa.be
    settingSalsaBeImportLooperSettings :: !LooperSettings,
    -- https://latinworld.nl
    settingLatinworldNlImportLooperSettings :: !LooperSettings,
    -- https://tanzagenda.ch
    settingTanzagendaChImportLooperSettings :: !LooperSettings
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
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
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
  let settingOrganiserReminderLooperSettings = deriveLooperSettings (seconds 30) (hours 24) flagOrganiserReminderLooperFlags envOrganiserReminderLooperEnvironment (mc confOrganiserReminderLooperConfiguration)
  let settingImageGarbageCollectorLooperSettings = deriveLooperSettings (minutes 1 + seconds 30) (hours 24) flagImageGarbageCollectorLooperFlags envImageGarbageCollectorLooperEnvironment (mc confImageGarbageCollectorLooperConfiguration)
  let settingPartySchedulerLooperSettings = deriveLooperSettings (minutes 2 + seconds 30) (hours 24) flagPartySchedulerLooperFlags envPartySchedulerLooperEnvironment (mc confPartySchedulerLooperConfiguration)
  let settingImporterInterval = maybe (hours 24) fromIntegral $ flagImporterInterval <|> envImporterInterval <|> mc confImporterInterval
  let settingEventsInfoImportLooperSettings = deriveLooperSettings (minutes 2 + seconds 1) (hours 1) flagEventsInfoImportLooperFlags envEventsInfoImportLooperEnvironment (mc confEventsInfoImportLooperConfiguration)
  let settingGolatindanceComImportLooperSettings = deriveLooperSettings (minutes 3 + seconds 2) (hours 1) flagGolatindanceComImportLooperFlags envGolatindanceComImportLooperEnvironment (mc confGolatindanceComImportLooperConfiguration)
  let settingDanceplaceComImportLooperSettings = deriveLooperSettings (minutes 4 + seconds 3) (hours 1) flagDanceplaceComImportLooperFlags envDanceplaceComImportLooperEnvironment (mc confDanceplaceComImportLooperConfiguration)
  let settingMapdanceComImportLooperSettings = deriveLooperSettings (minutes 5 + seconds 4) (hours 1) flagMapdanceComImportLooperFlags envMapdanceComImportLooperEnvironment (mc confMapdanceComImportLooperConfiguration)
  let settingSalsachicagoComImportLooperSettings = deriveLooperSettings (minutes 6 + seconds 5) (hours 1) flagSalsachicagoComImportLooperFlags envSalsachicagoComImportLooperEnvironment (mc confSalsachicagoComImportLooperConfiguration)
  let settingDancefloorfinderComImportLooperSettings = deriveLooperSettings (minutes 7 + seconds 6) (hours 1) flagDancefloorfinderComImportLooperFlags envDancefloorfinderComImportLooperEnvironment (mc confDancefloorfinderComImportLooperConfiguration)
  let settingSensualDanceImportLooperSettings = deriveLooperSettings (minutes 8 + seconds 7) (hours 1) flagSensualDanceImportLooperFlags envSensualDanceImportLooperEnvironment (mc confSensualDanceImportLooperConfiguration)
  let settingSalsaBeImportLooperSettings = deriveLooperSettings (minutes 9 + seconds 8) (hours 1) flagSalsaBeImportLooperFlags envSalsaBeImportLooperEnvironment (mc confSalsaBeImportLooperConfiguration)
  let settingLatinworldNlImportLooperSettings = deriveLooperSettings (minutes 10 + seconds 9) (hours 1) flagLatinworldNlImportLooperFlags envLatinworldNlImportLooperEnvironment (mc confLatinworldNlImportLooperConfiguration)
  let settingTanzagendaChImportLooperSettings = deriveLooperSettings (minutes 11 + seconds 10) (hours 1) flagTanzagendaChImportLooperFlags envTanzagendaChImportLooperEnvironment (mc confTanzagendaChImportLooperConfiguration)
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
    confOrganiserReminderLooperConfiguration :: !(Maybe LooperConfiguration),
    confImageGarbageCollectorLooperConfiguration :: !(Maybe LooperConfiguration),
    confPartySchedulerLooperConfiguration :: !(Maybe LooperConfiguration),
    confImporterInterval :: !(Maybe Int),
    confEventsInfoImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confGolatindanceComImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confDanceplaceComImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confMapdanceComImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confSalsachicagoComImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confDancefloorfinderComImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confSensualDanceImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confSalsaBeImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confLatinworldNlImportLooperConfiguration :: !(Maybe LooperConfiguration),
    confTanzagendaChImportLooperConfiguration :: !(Maybe LooperConfiguration)
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
        <*> optionalFieldOrNull "organiser-reminder" "The organiser reminder looper" .= confOrganiserReminderLooperConfiguration
        <*> optionalFieldOrNull "image-garbage-collector" "The image garbage collector looper" .= confImageGarbageCollectorLooperConfiguration
        <*> optionalFieldOrNull "party-scheduler" "The party scheduler looper" .= confPartySchedulerLooperConfiguration
        <*> optionalFieldOrNull "importer-interval" "The default interval for importers" .= confImporterInterval
        <*> optionalFieldOrNull "events-info-importer" "The events.info import looper" .= confEventsInfoImportLooperConfiguration
        <*> optionalFieldOrNull "golatindance-com-importer" "The golatindance.com import looper" .= confGolatindanceComImportLooperConfiguration
        <*> optionalFieldOrNull "danceplace-com-importer" "The danceplace.com import looper" .= confDanceplaceComImportLooperConfiguration
        <*> optionalFieldOrNull "mapdance-com-importer" "The mapdance.com import looper" .= confMapdanceComImportLooperConfiguration
        <*> optionalFieldOrNull "salsachicago-com-importer" "The salsachicago.com import looper" .= confSalsachicagoComImportLooperConfiguration
        <*> optionalFieldOrNull "dancefloorfinder-com-importer" "The dancefloorfinder.com import looper" .= confDancefloorfinderComImportLooperConfiguration
        <*> optionalFieldOrNull "sensual-dance-importer" "The sensual.dance import looper" .= confSensualDanceImportLooperConfiguration
        <*> optionalFieldOrNull "salsa-be-importer" "The salsa.be import looper" .= confSalsaBeImportLooperConfiguration
        <*> optionalFieldOrNull "latinworld-nl-importer" "The latinworld.nl import looper" .= confLatinworldNlImportLooperConfiguration
        <*> optionalFieldOrNull "tanzagenda-ch-importer" "The tanzagenda.ch import looper" .= confTanzagendaChImportLooperConfiguration

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
    envOrganiserReminderLooperEnvironment :: !LooperEnvironment,
    envImageGarbageCollectorLooperEnvironment :: !LooperEnvironment,
    envPartySchedulerLooperEnvironment :: !LooperEnvironment,
    envImporterInterval :: !(Maybe Int),
    envEventsInfoImportLooperEnvironment :: !LooperEnvironment,
    envGolatindanceComImportLooperEnvironment :: !LooperEnvironment,
    envDanceplaceComImportLooperEnvironment :: !LooperEnvironment,
    envMapdanceComImportLooperEnvironment :: !LooperEnvironment,
    envSalsachicagoComImportLooperEnvironment :: !LooperEnvironment,
    envDancefloorfinderComImportLooperEnvironment :: !LooperEnvironment,
    envSensualDanceImportLooperEnvironment :: !LooperEnvironment,
    envSalsaBeImportLooperEnvironment :: !LooperEnvironment,
    envLatinworldNlImportLooperEnvironment :: !LooperEnvironment,
    envTanzagendaChImportLooperEnvironment :: !LooperEnvironment
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
      <*> Env.var (fmap Just . Env.auto) "LOG_LEVEL" (mE <> Env.help "Minimal severity for log messages")
      <*> Env.var (fmap Just . Env.auto) "DATABASE" (mE <> Env.help "The path to the database file")
      <*> Env.var (fmap Just . Env.auto) "SEND_EMAILS" (mE <> Env.help "Whether to send emails and require email verification")
      <*> Env.var (fmap Just . Env.str) "SEND_ADDRESS" (mE <> Env.help "The address to send emails from")
      <*> Env.var (fmap Just . Env.str) "ADMIN" (mE <> Env.help "The email address of the admin user")
      <*> Env.var (fmap Just . Env.auto) "ENABLE_OSM_GEOCODING" (mE <> Env.help "Enable OpenStreetMaps Geocoding")
      <*> Env.var (fmap Just . Env.auto) "ENABLE_GOOGLE_GEOCODING" (mE <> Env.help "Enable Google Geocoding")
      <*> sentryEnvironmentParser
      <*> Env.var (fmap Just . Env.str) "GOOGLE_API_KEY" (mE <> Env.help "Google api key")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_ANALYTICS_TRACKING" (mE <> Env.help "Google analytics tracking code")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (mE <> Env.help "Google search console html element verification code")
      <*> looperEnvironmentParser "ORGANISER_REMINDER"
      <*> looperEnvironmentParser "IMAGE_GARBAGE_COLLECTOR"
      <*> looperEnvironmentParser "PARTY_SCHEDULER"
      <*> Env.var (fmap Just . Env.auto) "IMPORTER_INTERVAL" (mE <> Env.help "The default interval for the importers")
      <*> looperEnvironmentParser "EVENTS_INFO_IMPORTER"
      <*> looperEnvironmentParser "GOLATINDANCE_COM_IMPORTER"
      <*> looperEnvironmentParser "DANCEPLACE_COM_IMPORTER"
      <*> looperEnvironmentParser "MAPDANCE_COM_IMPORTER"
      <*> looperEnvironmentParser "SALSACHICAGO_COM_IMPORTER"
      <*> looperEnvironmentParser "DANCEFLOORFINDER_COM_IMPORTER"
      <*> looperEnvironmentParser "SENSUAL_DANCE_IMPORTER"
      <*> looperEnvironmentParser "SALSA_BE_IMPORTER"
      <*> looperEnvironmentParser "LATINWORLD_NL_IMPORTER"
      <*> looperEnvironmentParser "TANZAGENDA_CH_IMPORTER"
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
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
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
    flagOrganiserReminderLooperFlags :: !LooperFlags,
    flagImageGarbageCollectorLooperFlags :: !LooperFlags,
    flagPartySchedulerLooperFlags :: !LooperFlags,
    flagImporterInterval :: !(Maybe Int),
    flagEventsInfoImportLooperFlags :: !LooperFlags,
    flagGolatindanceComImportLooperFlags :: !LooperFlags,
    flagDanceplaceComImportLooperFlags :: !LooperFlags,
    flagMapdanceComImportLooperFlags :: !LooperFlags,
    flagSalsachicagoComImportLooperFlags :: !LooperFlags,
    flagDancefloorfinderComImportLooperFlags :: !LooperFlags,
    flagSensualDanceImportLooperFlags :: !LooperFlags,
    flagSalsaBeImportLooperFlags :: !LooperFlags,
    flagLatinworldNlImportLooperFlags :: !LooperFlags,
    flagTanzagendaChImportLooperFlags :: !LooperFlags
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
    <*> getLooperFlags "organiser-reminder"
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
    <*> getLooperFlags "events-info-importer"
    <*> getLooperFlags "golatindance-com-importer"
    <*> getLooperFlags "danceplace-com-importer"
    <*> getLooperFlags "mapdance-com-importer"
    <*> getLooperFlags "salsachicago-com-importer"
    <*> getLooperFlags "dancefloorfinder-com-importer"
    <*> getLooperFlags "sensual-dance-importer"
    <*> getLooperFlags "salsa-be-importer"
    <*> getLooperFlags "latinworld-nl-importer"
    <*> getLooperFlags "tanzagenda-ch-importer"

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
