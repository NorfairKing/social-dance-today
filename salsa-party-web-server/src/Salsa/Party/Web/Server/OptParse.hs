{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.OptParse where

import Control.Applicative
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
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
    settingGoogleAPIKey :: !(Maybe Text),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
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
  let settingGoogleAPIKey = flagGoogleAPIKey <|> envGoogleAPIKey <|> mc confGoogleAPIKey
  let settingGoogleAnalyticsTracking = flagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking
  let settingGoogleSearchConsoleVerification = flagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confDbFile :: !(Maybe FilePath),
    confSendEmails :: !(Maybe Bool),
    confAdmin :: !(Maybe Text),
    confGoogleAPIKey :: !(Maybe Text),
    confGoogleAnalyticsTracking :: !(Maybe Text),
    confGoogleSearchConsoleVerification :: !(Maybe Text)
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
        <*> optionalField "google-api-key" "Google API key"
        <*> optionalField "google-analytics-tracking" "Google analytics tracking code"
        <*> optionalField "google-search-console-verification" "Google search console html element verification code"

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
    envGoogleAPIKey :: !(Maybe Text),
    envGoogleAnalyticsTracking :: !(Maybe Text),
    envGoogleSearchConsoleVerification :: !(Maybe Text)
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
      <*> Env.var (fmap Just . Env.str) "GOOGLE_API_KEY" (mE <> Env.help "Google api key")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_ANALYTICS_TRACKING" (mE <> Env.help "Google analytics tracking code")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (mE <> Env.help "Google search console html element verification code")
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
    flagGoogleAPIKey :: !(Maybe Text),
    flagGoogleAnalyticsTracking :: !(Maybe Text),
    flagGoogleSearchConsoleVerification :: !(Maybe Text)
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
      ( switch
          ( mconcat
              [ long "send-emails",
                help "Whether to send emails and require email verification"
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
