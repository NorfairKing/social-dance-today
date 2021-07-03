{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer.Env where

import Conduit
import Control.Concurrent.TokenLimiter
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import System.Random (randomRIO)
import Text.Show.Pretty (ppShow)
import UnliftIO

data Importer = Importer
  { importerName :: Text,
    importerFunc :: Import ()
  }
  deriving (Generic)

runImporter :: App -> Importer -> LoggingT IO ()
runImporter a Importer {..} = runReaderT (unImport importerFunc) a

newtype Import a = Import {unImport :: ReaderT App (LoggingT IO) a}
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadReader App,
      MonadLoggerIO,
      MonadLogger,
      MonadIO,
      MonadThrow
    )

externalEventSink :: ConduitT ExternalEvent Void Import ()
externalEventSink = awaitForever $ \externalEvent@ExternalEvent {..} -> do
  now <- liftIO getCurrentTime
  lift $
    appDB $ do
      mExternalEvent <- getBy (UniqueExternalEventKey externalEventKey)
      case mExternalEvent of
        Nothing -> insert_ externalEvent
        Just (Entity externalEventId oldExternalEvent) ->
          if externalEvent `hasChangedComparedTo` oldExternalEvent
            then
              void $
                update
                  externalEventId
                  [ ExternalEventTitle =. externalEventTitle,
                    ExternalEventDescription =. externalEventDescription,
                    ExternalEventOrganiser =. externalEventOrganiser,
                    ExternalEventDay =. externalEventDay,
                    ExternalEventStart =. externalEventStart,
                    ExternalEventHomepage =. externalEventHomepage,
                    ExternalEventModified =. Just now,
                    ExternalEventPlace =. externalEventPlace,
                    ExternalEventOrigin =. externalEventOrigin
                  ]
            else pure ()

jsonRequestConduit :: FromJSON a => ConduitT HTTP.Request a Import ()
jsonRequestConduit = do
  man <- asks appHTTPManager
  userAgent <- liftIO chooseUserAgent
  let limitConfig =
        defaultLimitConfig
          { maxBucketTokens = 10, -- Ten tokens maximum, represents one request
            initialBucketTokens = 10,
            bucketRefillTokensPerSecond = 1
          }
  rateLimiter <- liftIO $ newRateLimiter limitConfig
  awaitForever $ \requestPrototype -> do
    liftIO $ waitDebit limitConfig rateLimiter 10 -- Need 10 tokens
    let request = requestPrototype {requestHeaders = ("User-Agent", userAgent) : requestHeaders requestPrototype}
    logInfoNS "Importer" $ "fetching: " <> T.pack (show (getUri request))
    errOrResponse <-
      liftIO $
        (Right <$> httpLbs request man)
          `catches` [ Handler $ \e -> pure (Left (toHttpException request e)),
                      Handler $ \e -> pure (Left (e :: HttpException))
                    ]
    case errOrResponse of
      Left err -> do
        logErrorNS "Importer" $
          T.unlines
            [ "HTTP Exception occurred.",
              "request:",
              T.pack (ppShow request),
              "exception:",
              T.pack (ppShow err)
            ]
      Right response -> do
        let body = responseBody response
        case JSON.eitherDecode body of
          Left err ->
            logErrorNS "Importer" $
              T.unlines
                [ "Invalid JSON:" <> T.pack err,
                  T.pack (show body)
                ]
          Right jsonValue ->
            case JSON.parseEither parseJSON jsonValue of
              Left err ->
                logErrorNS "Importer" $
                  T.unlines
                    [ "Unable to parse JSON:" <> T.pack err,
                      T.pack $ ppShow jsonValue
                    ]
              Right a -> yield a

chooseUserAgent :: IO ByteString
chooseUserAgent = do
  index <- randomRIO (0, length userAgentList - 1)
  pure $ userAgentList !! index

userAgentList :: [ByteString]
userAgentList =
  [ "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FSL 7.0.6.01001)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FSL 7.0.7.01001)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; FSL 7.0.5.01003)",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0",
    "Mozilla/5.0 (X11; U; Linux x86_64; de; rv:1.9.2.8) Gecko/20100723 Ubuntu/10.04 (lucid) Firefox/3.6.8",
    "Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:11.0) Gecko/20100101 Firefox/11.0",
    "Mozilla/5.0 (X11; U; Linux x86_64; de; rv:1.9.2.8) Gecko/20100723 Ubuntu/10.04 (lucid) Firefox/3.6.8",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.0.3705)",
    "Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:13.0) Gecko/20100101 Firefox/13.0.1",
    "Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)",
    "Opera/9.80 (Windows NT 5.1; U; en) Presto/2.10.289 Version/12.01",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727)",
    "Mozilla/5.0 (Windows NT 5.1; rv:5.0.1) Gecko/20100101 Firefox/5.0.1",
    "Mozilla/5.0 (Windows NT 6.1; rv:5.0) Gecko/20100101 Firefox/5.02",
    "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.112 Safari/535.1",
    "Mozilla/4.0 (compatible; MSIE 6.0; MSIE 5.5; Windows NT 5.0) Opera 7.02 Bork-edition [en]"
  ]
