{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Importer.Env where

import Conduit
import Control.Concurrent.TokenLimiter.Concurrent
import Control.Exception (AsyncException)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Retry
import Network.HTTP.Types as HTTP
import Network.URI
import Salsa.Party.AdminNotification
import Salsa.Party.DB
import Salsa.Party.Poster
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Geocoding
import System.Random (randomRIO)
import System.Random.Shuffle
import Text.HTML.Scalpel
import qualified Text.HTML.TagSoup as HTML
import Text.Show.Pretty (pPrint, ppShow)
import UnliftIO
import qualified Web.JSONLD as LD
import qualified Web.JSONLD.Parse as LD

data Importer = Importer
  { importerName :: !Text,
    importerFunc :: !(Import ()),
    -- | The user agent we will use for fetching, so we can help the organisers
    -- identify us.
    importerUserAgent :: !UserAgent,
    -- | The timezone of the people who made the site, so we can make sure not
    -- to bother them.
    importerTimezoneOffset :: !Int -- In hours
  }
  deriving (Generic)

data UserAgent
  = UserAgentSocial
  | UserAgentRandom
  deriving (Generic)

runImporter :: App -> Importer -> LoggingT IO ()
runImporter a Importer {..} = addImporterNameToLog importerName $ do
  let runDBHere :: SqlPersistT (LoggingT IO) a -> LoggingT IO a
      runDBHere = flip runSqlPool (appConnectionPool a) . retryOnBusy

  begin <- liftIO getCurrentTime
  Entity importerId _ <-
    runDBHere $
      upsertBy
        (UniqueImporterMetadataName importerName)
        ( ImporterMetadata
            { importerMetadataName = importerName,
              importerMetadataLastRunStart = Just begin,
              importerMetadataLastRunEnd = Nothing,
              importerMetadataLastRunImported = Just 0
            }
        )
        [ ImporterMetadataLastRunStart =. Just begin,
          ImporterMetadataLastRunEnd =. Nothing,
          ImporterMetadataLastRunImported =. Just 0
        ]

  userAgent <- case importerUserAgent of
    UserAgentSocial -> pure socialDanceUserAgent
    UserAgentRandom -> liftIO chooseUserAgent
  logDebugN $ T.pack $ unwords ["Chose user agent:", show userAgent]
  let tokenLimitConfig =
        TokenLimitConfig
          { tokenLimitConfigMaxTokens = 10, -- Ten tokens maximum, represents one request
            tokenLimitConfigInitialTokens = 0, -- Fetch almost-immediately at the start
            tokenLimitConfigTokensPerSecond = if development then 10 else 1
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

  errOrUnit <-
    (Right <$> runReaderT (unImport importerFunc) env)
      `catches` [
                  -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
                  Handler (\e -> throwIO (e :: AsyncException)),
                  -- Catch all the rest as a string
                  Handler (\e -> pure $ Left (e :: SomeException))
                ]
  case errOrUnit of
    Right () -> pure ()
    Left err -> do
      let message =
            T.pack $
              unlines
                [ unwords ["Importer threw an exception:", show importerName],
                  displayException err
                ]
      logErrorN message
      runReaderT (sendAdminNotification message) a

  end <- liftIO getCurrentTime
  -- We don't just use 'update' here because the admin could have deleted this metadata in the meantime.
  void $
    runDBHere $
      upsertBy
        (UniqueImporterMetadataName importerName)
        ( ImporterMetadata
            { importerMetadataName = importerName,
              importerMetadataLastRunStart = Just begin,
              importerMetadataLastRunEnd = Just end,
              importerMetadataLastRunImported = Nothing
            }
        )
        [ ImporterMetadataLastRunStart =. Just begin,
          ImporterMetadataLastRunEnd =. Just end
        ]

addImporterNameToLog :: Text -> LoggingT m a -> LoggingT m a
addImporterNameToLog importerName = modLogSource $ \source ->
  if T.null source
    then "importer-" <> importerName
    else source

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
      MonadThrow,
      MonadFail
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
  liftIO $ runLoggingT (runSqlPool (retryOnBusy func) pool) logFunc

importExternalEventWithMImage :: (ExternalEvent, Maybe URI) -> Import ()
importExternalEventWithMImage (externalEvent, mImageURI) =
  importExternalEventAnd externalEvent $ \externalEventId -> do
    now <- liftIO getCurrentTime
    forM_ mImageURI $ \imageUri -> do
      mImageKey <- tryToImportImage imageUri
      forM_ mImageKey $ \key -> do
        importDB $
          update
            externalEventId
            [ ExternalEventPoster =. Just key,
              ExternalEventModified =. Just now
            ]

importExternalEvent :: ExternalEvent -> Import ()
importExternalEvent externalEvent = importExternalEventAnd externalEvent $ \_ -> pure ()

-- Import an external event and run the given function if anything has changed.
-- We use this extra function to import images but only if the event has changed.
importExternalEventAnd :: ExternalEvent -> (ExternalEventId -> Import ()) -> Import ()
importExternalEventAnd candidate func = do
  logDebugN $
    T.pack $
      unlines
        [ "Considering importing external event:",
          ppShow candidate
        ]
  case prettyValidate candidate of
    Left err ->
      logWarnN $
        T.pack $
          unlines
            [ "Not importing invalid event:",
              err,
              ppShow candidate
            ]
    Right externalEvent@ExternalEvent {..} -> do
      now <- liftIO getCurrentTime
      let today = utctDay now
      let yesterday = addDays (-1) today
      if externalEventDay < yesterday
        then
          logDebugN $
            T.pack $
              unwords
                [ "Not importing external event because it is in the past:",
                  T.unpack externalEventOrigin
                ]
        else do
          importerId <- asks importEnvId
          mExternalEvent <- importDB $ do
            update importerId [ImporterMetadataLastRunImported +=. Just 1]
            getBy (UniqueExternalEventKey importerId externalEventKey)
          case mExternalEvent of
            Nothing -> do
              logInfoN $
                T.pack $
                  unwords
                    [ "Importing never-before-seen event from",
                      T.unpack externalEventOrigin
                    ]
              importDB (insert externalEvent) >>= func
            Just (Entity externalEventId oldExternalEvent) -> do
              case externalEvent `changesComparedTo` oldExternalEvent of
                Nothing -> do
                  logInfoN $
                    T.pack $
                      unwords
                        [ "Not re-importing known event, because it was not changed",
                          show externalEventOrigin
                        ]
                  pure ()
                Just updates -> do
                  logInfoN $
                    T.pack $
                      unwords
                        [ "Importing known-but-changed event from",
                          show externalEventOrigin
                        ]
                  importDB $
                    void $
                      update
                        externalEventId
                        ( (ExternalEventModified =. Just now) :
                          NE.toList updates
                        )
                  func externalEventId

jsonRequestConduit :: FromJSON a => ConduitT HTTP.Request a Import ()
jsonRequestConduit = C.map ((,) ()) .| jsonRequestConduitWith .| C.map snd

jsonRequestConduit' :: FromJSON a => ConduitT HTTP.Request (HTTP.Request, a) Import ()
jsonRequestConduit' = C.map (\r -> (r, r)) .| jsonRequestConduitWith

jsonRequestConduitWith :: FromJSON a => ConduitT (c, HTTP.Request) (c, a) Import ()
jsonRequestConduitWith = awaitForever $ \(c, request) -> do
  errOrResponse <- lift $ doHttpRequest request
  case errOrResponse of
    Left err ->
      logErrorN $
        T.pack $
          unlines
            [ "HTTP Exception occurred.",
              "request:",
              ppShow request,
              "exception:",
              ppShow err
            ]
    Right response -> do
      let body = responseBody response
      case JSON.eitherDecode body of
        Left err ->
          logErrorN $
            T.pack $
              unlines
                [ unwords ["Invalid JSON:", err],
                  show body,
                  err
                ]
        Right jsonValue ->
          case JSON.parseEither parseJSON jsonValue of
            Left err ->
              logErrorN $
                T.pack $
                  unlines
                    [ unwords ["Unable to parse JSON:", err],
                      case TE.decodeUtf8' (LB.toStrict (JSON.encodePretty jsonValue)) of
                        Left _ -> "non-utf8, somehow"
                        Right t -> show t,
                      err
                    ]
            Right a -> yield (c, a)

debugConduit :: (Show a, MonadIO m) => ConduitT a a m ()
debugConduit = C.mapM $ \a -> do
  liftIO $ pPrint a
  pure a

doHttpRequestWith :: ConduitT HTTP.Request (HTTP.Request, Either HttpException (HTTP.Response LB.ByteString)) Import ()
doHttpRequestWith = C.mapM (\req -> (,) req <$> doHttpRequest req)

doHttpRequestWith' :: ConduitT (a, HTTP.Request) (a, HTTP.Request, Either HttpException (HTTP.Response LB.ByteString)) Import ()
doHttpRequestWith' = C.mapM (\(a, req) -> (,,) a req <$> doHttpRequest req)

doHttpRequest :: HTTP.Request -> Import (Either HttpException (HTTP.Response LB.ByteString))
doHttpRequest requestPrototype = do
  man <- asks $ appHTTPManager . importEnvApp
  userAgent <- asks importEnvUserAgent
  let oldHeaders = requestHeaders requestPrototype
      newHeaders = case lookup "User-Agent" oldHeaders of
        Nothing -> ("User-Agent", userAgent) : oldHeaders
        Just _ -> oldHeaders
  let request = requestPrototype {requestHeaders = newHeaders}
  waitToFetch (show (getUri request))
  logInfoN $ T.pack $ unwords ["Fetching:", show (getUri request)]
  httpLbsWithRetry request man

waitToFetch :: String -> Import ()
waitToFetch uri = do
  logDebugN $ T.pack $ unwords ["Waiting to fetch:", show uri]
  tokenLimiter <- asks importEnvRateLimiter
  liftIO $ waitDebit tokenLimiter 10 -- Need 10 tokens

setUserAgent :: ByteString -> Request -> Request
setUserAgent userAgent requestPrototype =
  let oldHeaders = requestHeaders requestPrototype
      newHeaders = case lookup hUserAgent oldHeaders of
        Nothing -> (hUserAgent, userAgent) : oldHeaders
        Just _ -> oldHeaders
   in requestPrototype {requestHeaders = newHeaders}

chooseUserAgent :: IO ByteString
chooseUserAgent = do
  index <- randomRIO (0, length userAgentList - 1)
  pure $ userAgentList !! index

-- https://techblog.willshouse.com/2012/01/03/most-common-user-agents/
-- Last Updated: Sun, 05 Jun 2022 10:08:39 +0000
userAgentList :: [ByteString]
userAgentList =
  [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.67 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:100.0) Gecko/20100101 Firefox/100.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64; rv:100.0) Gecko/20100101 Firefox/100.0",
    "Mozilla/5.0 (Windows NT 10.0; rv:91.0) Gecko/20100101 Firefox/91.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.4 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.63 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:100.0) Gecko/20100101 Firefox/100.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36 Edg/101.0.1210.53",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.5 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.0.0 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.62 Safari/537.36",
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:100.0) Gecko/20100101 Firefox/100.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36 Edg/101.0.1210.47",
    "Mozilla/5.0 (X11; Linux x86_64; rv:101.0) Gecko/20100101 Firefox/101.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Safari/537.36 Edg/101.0.1210.39",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:99.0) Gecko/20100101 Firefox/99.0",
    "Mozilla/5.0 (X11; Linux x86_64; rv:91.0) Gecko/20100101 Firefox/91.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.3 Safari/605.1.15",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.75 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64; rv:99.0) Gecko/20100101 Firefox/99.0",
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:99.0) Gecko/20100101 Firefox/99.0",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:100.0) Gecko/20100101 Firefox/100.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36 OPR/86.0.4363.59",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.41 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.41 Safari/537.36 Edg/101.0.1210.32",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0.4758.141 YaBrowser/22.3.3.852 Yowser/2.5 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.88 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:91.0) Gecko/20100101 Firefox/91.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.1 Safari/605.1.15",
    "Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:100.0) Gecko/20100101 Firefox/100.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.88 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.60 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.41 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:101.0) Gecko/20100101 Firefox/101.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:99.0) Gecko/20100101 Firefox/99.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.88 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.2 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.67 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.41 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.2 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36 OPR/86.0.4363.64",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.71",
    "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.67 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64; rv:78.0) Gecko/20100101 Firefox/78.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.4 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.143 YaBrowser/22.5.0.1814 Yowser/2.5 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.67 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.67 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:100.0) Gecko/20100101 Firefox/100.0"
  ]

tryToImportImage :: URI -> Import (Maybe CASKey)
tryToImportImage uri = do
  case requestFromURI uri of
    Nothing -> pure Nothing
    Just request -> do
      logInfoN $ T.pack $ unwords ["Importing image:", show uri]
      errOrResponse <- doHttpRequest request
      case errOrResponse of
        Left _ -> pure Nothing
        Right response -> do
          case TE.decodeUtf8' $ fromMaybe "image/jpeg" $ lookup "Content-Type" (responseHeaders response) of
            Left _ -> pure Nothing
            Right contentType -> do
              let imageBlob = LB.toStrict $ responseBody response
              tryToImportImageBlob contentType imageBlob

tryToImportImageBlob :: Text -> ByteString -> Import (Maybe CASKey)
tryToImportImageBlob contentType imageBlob =
  case posterCropImage contentType imageBlob of
    Left err -> do
      logErrorN $ T.pack $ unwords ["Error while trying to import image:", err]
      pure Nothing
    Right (convertedImageType, convertedImageBlob) -> do
      let casKey = mkCASKey convertedImageType convertedImageBlob
      now <- liftIO getCurrentTime
      _ <-
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
      pure $ Just casKey

logRequestErrors ::
  ConduitT
    (HTTP.Request, Either HttpException (Response LB.ByteString))
    (HTTP.Request, Response LB.ByteString)
    Import
    ()
logRequestErrors = awaitForever $ \(request, errOrResponse) -> case errOrResponse of
  Left err -> logErrorN $ T.pack $ unlines ["Error while fetching page:", ppShow err]
  Right response -> yield (request, response)

logRequestErrors' ::
  ConduitT
    (a, HTTP.Request, Either HttpException (Response LB.ByteString))
    (a, HTTP.Request, Response LB.ByteString)
    Import
    ()
logRequestErrors' = awaitForever $ \(a, request, errOrResponse) -> case errOrResponse of
  Left err -> logErrorN $ T.pack $ unlines ["Error while fetching page:", ppShow err]
  Right response -> yield (a, request, response)

httpBodyTextParserC :: Monad m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) (HTTP.Request, HTTP.Response Text) m ()
httpBodyTextParserC = C.concatMap (traverse parseHttpBodyText) -- This uses Foldable ((,) a).

parseHttpBodyText :: HTTP.Response LB.ByteString -> Either TE.UnicodeException (HTTP.Response Text)
parseHttpBodyText response =
  let headers = HTTP.responseHeaders response
      contentType = lookup hContentType headers
      iso88591Decoder = pure . TE.decodeLatin1
      decoder :: ByteString -> Either TE.UnicodeException Text
      decoder = case contentType of
        Nothing -> iso88591Decoder
        Just ct ->
          if "charset=UTF-8" `SB.isInfixOf` ct
            then TE.decodeUtf8'
            else iso88591Decoder
   in traverse (decoder . LB.toStrict) response

teePrint ::
  (Show a, MonadIO m) => ConduitT a a m ()
teePrint = C.mapM (\a -> liftIO $ pPrint a >> pure a)

deduplicateC :: forall a m. (Show a, Ord a, MonadLogger m) => ConduitT a a m ()
deduplicateC = () <$ go S.empty
  where
    go :: Set a -> ConduitT a a m (Set a)
    go seen = do
      ma <- await
      case ma of
        Nothing -> pure seen
        Just a ->
          if S.member a seen
            then do
              logDebugN $ T.pack $ unwords ["Already seen, not yielding:", show a]
              go seen
            else do
              logDebugN $ T.pack $ unwords ["Not seen yet, yielding:", show a]
              yield a
              go $ S.insert a seen

andDays :: MonadIO m => ConduitT a (a, Day) m ()
andDays = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let days = [today .. addDays daysToImportAhead today]
  awaitForever $ \a -> yieldMany $ map ((,) a) days

jsonLDEventsC ::
  ConduitT
    (Request, Response LB.ByteString)
    (HTTP.Request, HTTP.Response LB.ByteString, LD.Event)
    Import
    ()
jsonLDEventsC = parseJSONLDPieces .| parseJSONLDEvents

parseJSONLDPieces :: ConduitT (Request, Response LB.ByteString) (Request, Response LB.ByteString, JSON.Value) Import ()
parseJSONLDPieces = awaitForever $ \(request, response) -> do
  let c = HTTP.statusCode (responseStatus response)
  when (200 <= c && c < 300) $ do
    let values = fromMaybe [] $ scrapeStringLike (responseBody response) LD.scrapeJSONLDValues
    yieldManyShuffled $ map (\value -> (request, response, value)) values

parseJSONLDEvents ::
  ConduitT
    (HTTP.Request, HTTP.Response LB.ByteString, JSON.Value)
    (HTTP.Request, HTTP.Response LB.ByteString, LD.Event)
    Import
    ()
parseJSONLDEvents = awaitForever $ \(request, response, value) ->
  -- Try to parse as a single event first
  case JSON.parseEither parseJSON value of
    Right event -> yield (request, response, event)
    Left errorMessageSingle -> do
      -- Couldn't parse as a single event, check to
      -- warn about our parser potentially being broken.
      let typeParser :: Value -> JSON.Parser Text
          typeParser = withObject "ValueWithType" $ \o -> o .: "@type"
      let eventTypes = ["Event", "DanceEvent", "SocialEvent"]
      case JSON.parseMaybe typeParser value of
        Just typ ->
          if typ `elem` eventTypes
            then
              logWarnN $
                T.pack $
                  unlines
                    [ unwords ["Found a json object with '@type'", show typ, " but couldn't parse it as an event:"],
                      errorMessageSingle,
                      T.unpack $ LT.toStrict $ LTB.toLazyText $ JSON.encodePrettyToTextBuilder value
                    ]
            else pure ()
        Nothing ->
          case JSON.parseEither parseJSON value of
            Right events -> yieldMany $ map ((,,) request response) events
            Left errorMessageList -> do
              let listTypeParser :: Value -> JSON.Parser [Text]
                  listTypeParser value_ = do
                    list <- parseJSON value_
                    mapM typeParser list
              case JSON.parseMaybe listTypeParser value of
                Nothing -> pure ()
                Just types ->
                  if all (`elem` eventTypes) types
                    then
                      logWarnN $
                        T.pack $
                          unlines
                            [ "Found a list of json objects that all have '@type' that look like an event, but couldn't parse it as a list of events:",
                              errorMessageList,
                              T.unpack $ LT.toStrict $ LTB.toLazyText $ JSON.encodePrettyToTextBuilder value
                            ]
                    else pure ()

-- A bit more than one month ahead.
-- We don't care about importing _too_ far ahead
daysToImportAhead :: Integer
daysToImportAhead = 45

yieldManyShuffled :: MonadIO m => [a] -> ConduitT void a m ()
yieldManyShuffled list = do
  shuffledList <- shuffleList list
  yieldMany shuffledList

shuffleList :: MonadIO m => [a] -> m [a]
shuffleList = liftIO . shuffleM

convertLDEventToExternalEvent :: Text -> ConduitT (HTTP.Request, HTTP.Response LB.ByteString, LD.Event) (ExternalEvent, Maybe URI) Import ()
convertLDEventToExternalEvent keyPrefix = convertLDEventToExternalEventWith $ \request _ ->
  let uriText = T.pack $ show $ getUri request
   in case T.stripPrefix keyPrefix uriText of
        Nothing -> uriText
        Just suffix -> suffix

convertLDEventToExternalEventWith ::
  (HTTP.Request -> LD.Event -> Text) -> ConduitT (HTTP.Request, HTTP.Response LB.ByteString, LD.Event) (ExternalEvent, Maybe URI) Import ()
convertLDEventToExternalEventWith makeKey = awaitForever $ \(request, _, ldEvent) -> do
  let (externalEventDay, externalEventStart) = case LD.eventStartDate ldEvent of
        LD.EventStartDate d -> (LD.dateDay d, Nothing)
        LD.EventStartDateTime dateTime ->
          let LocalTime d tod = LD.dateTimeLocalTime dateTime
           in (d, Just tod)

  -- Check for day here already so we don't do any unnecessary place lookups.
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = addDays (-1) today
  when (externalEventDay >= yesterday) $ do
    mPlaceEntity <- lift $ geocodeLDEventLocation $ LD.eventLocation ldEvent
    case mPlaceEntity of
      Nothing -> logWarnN "Place not found."
      Just (Entity externalEventPlace _) -> do
        externalEventUuid <- nextRandomUUID
        let externalEventKey = makeKey request ldEvent
        let externalEventTitle = unescapeHtml $ LD.eventName ldEvent
        let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
        let externalEventDescription = unescapeHtml <$> LD.eventDescription ldEvent
        let externalEventOrganiser = do
              eventOrganizer <- LD.eventOrganizer ldEvent
              case eventOrganizer of
                LD.EventOrganizerOrganization organization -> pure $ LD.organizationName organization
        let externalEventHomepage = Nothing
        let externalEventPrice = Nothing
        let externalEventCancelled = case LD.eventStatus ldEvent of
              Just LD.EventCancelled -> Just True
              _ -> Nothing
        now <- liftIO getCurrentTime
        let externalEventPoster = Nothing
        let externalEventCreated = now
        let externalEventModified = Nothing
        externalEventImporter <- asks importEnvId
        let externalEventOrigin = T.pack $ show $ getUri request
        let mImageURI = do
              eventImage <- listToMaybe (LD.eventImages ldEvent)
              case eventImage of
                LD.EventImageURL t -> parseURI $ T.unpack t

        yield (ExternalEvent {..}, mImageURI)

geocodeLDEventLocation :: LD.EventLocation -> Import (Maybe (Entity Place))
geocodeLDEventLocation = \case
  LD.EventLocationPlace place -> geocodeLDPlace place

geocodeLDPlace :: LD.Place -> Import (Maybe (Entity Place))
geocodeLDPlace ldPlace = do
  let address = case LD.placeAddress ldPlace of
        LD.PlaceAddressText t -> unescapeHtml t
        LD.PlaceAddressPostalAddress postalAddress ->
          unescapeHtml $
            T.unwords $
              catMaybes
                [ LD.postalAddressStreetAddress postalAddress,
                  LD.postalAddressLocality postalAddress,
                  LD.postalAddressRegion postalAddress,
                  LD.postalAddressCountry postalAddress
                ]
  case LD.placeGeo ldPlace of
    Just (LD.PlaceGeoCoordinates geoCoordinates) -> do
      let place =
            Place
              { placeQuery = address,
                placeLat = LD.geoCoordinatesLatitude geoCoordinates,
                placeLon = LD.geoCoordinatesLongitude geoCoordinates
              }
      -- TODO consider double-checking coordinates.
      importPlaceWithSpecifiedCoordinates place
    Nothing -> do
      app <- asks importEnvApp
      runReaderT (lookupPlaceRaw address) app

importPlaceWithSpecifiedCoordinates :: Place -> Import (Maybe (Entity Place))
importPlaceWithSpecifiedCoordinates place =
  case prettyValidate place of
    Left err -> do
      logErrorN $
        T.pack $
          unlines
            [ unwords
                [ "Importing address",
                  show $ placeQuery place,
                  "with specified coordinates",
                  show $ placeCoordinates place,
                  "from LD Place failed because it produced an invalid place"
                ],
              err
            ]
      pure Nothing
    Right _ -> do
      fmap Just $
        importDB $
          upsertBy
            (UniquePlaceQuery $ placeQuery place)
            place
            [] -- Don't change if it's already there, so that they can't fill our page with junk.

unescapeHtml :: Text -> Text
unescapeHtml = HTML.innerText . HTML.parseTags

-- There seems to be a bug in tagsoup, that makes it so that characters that do not fit
-- into latin1, when utf8-encoded as HTML entities, are truncated when parsed.
-- Here is an example:
-- > HTML.parseTags ("&#128512;" :: ByteString)
-- [TagText "\NUL"]
-- > HTML.parseTags ("&#128512;" :: Text)
-- [TagText "\128512"]
unHTMLText :: LB.ByteString -> Text
unHTMLText =
  HTML.innerText
    . HTML.parseTags
    . decodeBytestringPessimistically
    . LB.toStrict

unHTMLAttribute :: LB.ByteString -> Text
unHTMLAttribute = decodeBytestringPessimistically . LB.toStrict

decodeBytestringPessimistically :: ByteString -> Text
decodeBytestringPessimistically strictBS =
  case TE.decodeUtf8' strictBS of
    Left _ -> TE.decodeLatin1 strictBS
    Right t -> t
