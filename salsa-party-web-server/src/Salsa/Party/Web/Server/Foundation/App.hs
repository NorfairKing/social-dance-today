module Salsa.Party.Web.Server.Foundation.App where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad.Logger
import Data.Text (Text)
import Data.Validity.Text ()
import Data.Validity.Time ()
import Database.Persist.Sql
import Network.HTTP.Client as HTTP
import Path
import Salsa.Party.DB
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Handler.Search.Types
import Yesod.EmbeddedStatic (EmbeddedStatic)

data App = App
  { appRoot :: !(Maybe Text),
    appLogLevel :: !LogLevel,
    appHashDifficulty :: !Int,
    appStatic :: !EmbeddedStatic,
    appHTTPManager :: !HTTP.Manager,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appSendEmails :: !Bool,
    appSendAddress :: !(Maybe Text),
    appSearchResultCache :: !SearchResultCache,
    appAdmin :: !(Maybe EmailAddress),
    appOSMRateLimiter :: !(Maybe TokenLimiter), -- Nothing means disabled.
    appSentrySettings :: !(Maybe SentrySettings), -- Nothing means disabled.
    appGoogleAPIKey :: !(Maybe Text), -- Nothing means disabled.
    appGoogleAnalyticsTracking :: !(Maybe Text), -- Nothing means disabled.
    appGoogleSearchConsoleVerification :: !(Maybe Text) -- Nothing means disabled.
  }
