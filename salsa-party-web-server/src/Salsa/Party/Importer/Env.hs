{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer.Env where

import Conduit
import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist
import Database.Persist.Sql
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Looper
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Retry
import Network.URI
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Poster
import System.Random (randomRIO)
import Text.Printf
import Text.Show.Pretty (ppShow)
import UnliftIO

data Importer = Importer
  { importerName :: Text,
    importerFunc :: Import ()
  }
  deriving (Generic)

-- We double-check whether to run the importer because we don't want to
-- bash any external sites should the importers or the webserver
-- crashloop, or we just deploy more often than once a day.
--
-- This does mean that if we deploy often, importers might be stopped early and not re-run,
-- but this shouldn't be a problem because:
--
--  * We try to fetch events at least a month in advance
--  * There should be at least once day in a month where we don't deploy twice a day.
runImporterWithDoubleCheck :: App -> LooperSettings -> Importer -> LoggingT IO ()
runImporterWithDoubleCheck app LooperSettings {..} importer = addImporterNameToLog (importerName importer) $ do
  let runDBHere :: SqlPersistT (LoggingT IO) a -> LoggingT IO a
      runDBHere = flip runSqlPool (appConnectionPool app)

  logInfoN "Checking whether to run"
  now <- liftIO getCurrentTime
  mImporterMetadata <- runDBHere $ getBy $ UniqueImporterMetadataName $ importerName importer
  let mLastRun = importerMetadataLastRunStart . entityVal <$> mImporterMetadata
  shouldRun <- case mLastRun of
    Nothing -> do
      logDebugN "Definitely running because it's never run before"
      pure True
    Just lastRun -> do
      let diff = diffUTCTime now lastRun
      let shouldRun = diff >= looperSetPeriod
          showDiffTime = T.pack . printf "%.0f" . (realToFrac :: NominalDiffTime -> Double)
      let ctx =
            T.unwords
              [ "because the last run was",
                T.pack (show lastRun),
                "which is",
                showDiffTime diff,
                "seconds ago and the looper period is",
                showDiffTime looperSetPeriod,
                "seconds"
              ]
      if shouldRun
        then logDebugN $ "Running " <> ctx
        else logDebugN $ "Not running " <> ctx
      pure shouldRun
  when shouldRun $ do
    logInfoN "Starting"
    begin <- liftIO getMonotonicTimeNSec
    runImporter app importer
    end <- liftIO getMonotonicTimeNSec
    logInfoN $ T.pack $ printf "Done, took %.2f seconds" (fromIntegral (end - begin) / (1_000_000_000 :: Double))

runImporter :: App -> Importer -> LoggingT IO ()
runImporter a Importer {..} = do
  let runDBHere :: SqlPersistT (LoggingT IO) a -> LoggingT IO a
      runDBHere = flip runSqlPool (appConnectionPool a)

  begin <- liftIO getCurrentTime
  Entity importerId _ <-
    runDBHere $
      upsertBy
        (UniqueImporterMetadataName importerName)
        ( ImporterMetadata
            { importerMetadataName = importerName,
              importerMetadataLastRunStart = begin,
              importerMetadataLastRunEnd = Nothing
            }
        )
        [ImporterMetadataLastRunStart =. begin]

  userAgent <- liftIO chooseUserAgent
  let tokenLimitConfig =
        TokenLimitConfig
          { tokenLimitConfigMaxTokens = 10, -- Ten tokens maximum, represents one request
            tokenLimitConfigInitialTokens = 10,
            tokenLimitConfigTokensPerSecond = 1
          }
  tokenLimiter <- liftIO $ makeTokenLimiter tokenLimitConfig
  let env =
        ImportEnv
          { importEnvApp = a,
            importEnvName = importerName,
            importEnvId = importerId,
            importEnvUserAgent = userAgent,
            importEnvRateLimiter = tokenLimiter
          }

  runReaderT
    (unImport importerFunc)
    env

  end <- liftIO getCurrentTime
  -- We don't just use 'update' here because the admin could have deleted this metadata in the meantime.
  void $
    runDBHere $
      upsertBy
        (UniqueImporterMetadataName importerName)
        ( ImporterMetadata
            { importerMetadataName = importerName,
              importerMetadataLastRunStart = begin,
              importerMetadataLastRunEnd = Just end
            }
        )
        [ ImporterMetadataLastRunStart =. begin,
          ImporterMetadataLastRunEnd =. Just end
        ]

addImporterNameToLog :: Text -> LoggingT m a -> LoggingT m a
addImporterNameToLog importerName = modLogSource $ \source -> if source == "" then "importer-" <> importerName else source

modLogSource :: (LogSource -> LogSource) -> LoggingT m a -> LoggingT m a
modLogSource func (LoggingT mFunc) = LoggingT $ \logFunc ->
  let newLogFunc loc source level str =
        let source' = func source
         in logFunc loc source' level str
   in mFunc newLogFunc

newtype Import a = Import {unImport :: ReaderT ImportEnv (LoggingT IO) a}
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadReader ImportEnv,
      MonadLoggerIO,
      MonadLogger,
      MonadIO,
      MonadUnliftIO,
      MonadThrow
    )

data ImportEnv = ImportEnv
  { importEnvApp :: !App,
    importEnvName :: !Text,
    importEnvId :: !ImporterMetadataId,
    importEnvUserAgent :: !ByteString,
    importEnvRateLimiter :: !TokenLimiter
  }

importDB :: SqlPersistT (LoggingT IO) a -> Import a
importDB func = do
  pool <- asks $ appConnectionPool . importEnvApp
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool func pool) logFunc

importExternalEvent :: ExternalEvent -> Import ()
importExternalEvent externalEvent = importExternalEventAnd externalEvent $ \_ -> pure ()

-- Import an external event and run the given function if anything has changed.
-- We use this extra function to import images but only if the event has changed.
importExternalEventAnd :: ExternalEvent -> (ExternalEventId -> Import ()) -> Import ()
importExternalEventAnd externalEvent@ExternalEvent {..} func = do
  now <- liftIO getCurrentTime
  importerId <- asks importEnvId
  mExternalEvent <- importDB $ getBy (UniqueExternalEventKey (Just importerId) externalEventKey)
  case mExternalEvent of
    Nothing -> importDB (insert externalEvent) >>= func
    Just (Entity externalEventId oldExternalEvent) -> do
      if externalEvent `hasChangedComparedTo` oldExternalEvent
        then do
          importDB $
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
                  ExternalEventOrigin =. externalEventOrigin,
                  ExternalEventImporter =. Just importerId
                ]
          func externalEventId
        else pure ()

jsonRequestConduit :: FromJSON a => ConduitT HTTP.Request a Import ()
jsonRequestConduit = C.map ((,) ()) .| jsonRequestConduitWith .| C.map snd

jsonRequestConduitWith :: FromJSON a => ConduitT (c, HTTP.Request) (c, a) Import ()
jsonRequestConduitWith = awaitForever $ \(c, request) -> do
  errOrResponse <- lift $ doHttpRequest request
  case errOrResponse of
    Left err ->
      logErrorN $
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
          logErrorN $
            T.unlines
              [ "Invalid JSON:" <> T.pack err,
                T.pack (show body)
              ]
        Right jsonValue ->
          case JSON.parseEither parseJSON jsonValue of
            Left err ->
              logErrorN $
                T.unlines
                  [ "Unable to parse JSON:" <> T.pack err,
                    T.pack $ ppShow jsonValue
                  ]
            Right a -> yield (c, a)

doHttpRequestWith :: ConduitT HTTP.Request (HTTP.Request, Either HttpException (HTTP.Response LB.ByteString)) Import ()
doHttpRequestWith = C.mapM (\req -> (,) req <$> doHttpRequest req)

doHttpRequest :: HTTP.Request -> Import (Either HttpException (HTTP.Response LB.ByteString))
doHttpRequest requestPrototype = do
  man <- asks $ appHTTPManager . importEnvApp
  userAgent <- asks importEnvUserAgent
  tokenLimiter <- asks importEnvRateLimiter
  liftIO $ waitDebit tokenLimiter 10 -- Need 10 tokens
  let request = requestPrototype {requestHeaders = ("User-Agent", userAgent) : requestHeaders requestPrototype}
  logInfoN $ "fetching: " <> T.pack (show (getUri request))
  httpLbsWithRetry request man

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

tryToImportImage :: URI -> Import (Maybe ImageId)
tryToImportImage uri = do
  case requestFromURI uri of
    Nothing -> pure Nothing
    Just request -> do
      errOrResponse <- doHttpRequest request
      case errOrResponse of
        Left _ -> pure Nothing
        Right response -> do
          case TE.decodeUtf8' $ fromMaybe "image/jpeg" $ lookup "Content-Type" (responseHeaders response) of
            Left _ -> pure Nothing
            Right contentType -> do
              let imageBlob = LB.toStrict $ responseBody response
              case posterCropImage contentType imageBlob of
                Left _ -> pure Nothing -- TODO log error
                Right (convertedImageType, convertedImageBlob) -> do
                  let casKey = mkCASKey convertedImageType convertedImageBlob
                  now <- liftIO getCurrentTime
                  Entity imageId _ <-
                    importDB $
                      upsertBy
                        (UniqueImageKey casKey)
                        ( Image
                            { imageKey = casKey,
                              imageTyp = convertedImageType,
                              imageBlob = convertedImageBlob,
                              imageCreated = now
                            }
                        )
                        [] -- No need to update anything, the casKey makes the image unique.
                  pure $ Just imageId

logRequestErrors ::
  ConduitT
    (HTTP.Request, Either HttpException (Response LB.ByteString))
    (HTTP.Request, Response LB.ByteString)
    Import
    ()
logRequestErrors = awaitForever $ \(request, errOrResponse) -> case errOrResponse of
  Left err -> logErrorN $ T.pack $ unlines ["Error while fetching page: " <> ppShow err]
  Right response -> yield (request, response)
