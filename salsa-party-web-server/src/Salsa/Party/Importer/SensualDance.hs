{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://sensual.dance
--
-- 1. There are no terms of service.
-- 2. The robots.txt does not forbid crawling.
--
-- This page is a real nightmare.
-- It only queries the parties at runtime, via javascript, using a graphql client that uses authentication.
module Salsa.Party.Importer.SensualDance (sensualDanceImporter) where

import Conduit
import Control.Concurrent.TokenLimiter.Concurrent
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Either
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String.QQ
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Test.WebDriver as WD
import Test.WebDriver.Capabilities as WD
import Test.WebDriver.Class as WD
import Test.WebDriver.Commands.Internal as WD
import Test.WebDriver.Commands.Wait as WD
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import Text.Read (readMaybe)

sensualDanceImporter :: Importer
sensualDanceImporter =
  Importer
    { importerName = "sensual.dance",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "http://sensual.dance/"
      .| fetchHome
      -- .| decodeLogEntries
      -- .| decodeInterestingLogEntries
      -- .| filterInterestingLogEntries
      -- .| doHttpRequestWith
      .| C.mapM_ (liftIO . pPrint)

fetchHome :: ConduitT String String Import ()
fetchHome = awaitForever $ \homeUrl -> do
  -- lift $ waitToFetch homeUrl
  lift $ logDebugN $ T.pack $ "Fetching: " <> homeUrl
  liftIO $ do
    let browser = WD.chrome {chromeOptions = ["--headless"]}
        caps =
          WD.defaultCaps
            { browser = browser,
              javascriptEnabled = Just True,
              additionalCaps =
                [ ( "loggingPrefs",
                    JSON.object
                      [ "performance"
                          .= ( "ALL" :: Text
                             )
                      ]
                  )
                ]
            }
        config = WD.defaultConfig {wdCapabilities = caps}
    WD.runSession config . WD.finallyClose $ do
      logTypes <- getLogTypes
      WD.setImplicitWait 10_000 -- Ten seconds
      WD.setScriptTimeout 10_000 -- Ten seconds
      WD.setPageLoadTimeout 10_000 -- Ten seconds
      WD.openPage homeUrl
      ready <- WD.waitUntil 10 $ do
        t <- executeJS [] "return document.readyState"
        WD.expect $ (t :: Text) == "complete"
      -- TODO make this delay longer in prod
      liftIO $ threadDelay 2_000_000 -- Wait for the entire page to be loaded.
      logEntries <- getLogs "performance"
      let logValues = rights $ map decodeLogEntry logEntries
      liftIO $ pPrint logValues
      let interestingLogEntries = rights $ map parseInterestingLogEntry logValues
      liftIO $ pPrint interestingLogEntries
      forM_ interestingLogEntries $ \InterestingLog {..} -> do
        let arg :: JSON.Value
            arg =
              object
                [ "cmd" .= ("Network.getResponseBody" :: Text),
                  "params"
                    .= object
                      [ "requestId" .= interestingLogRequestId
                      ]
                ]
        val <- doSessCommand methodPost "/goog/cdp/execute" arg
        liftIO $ logDebugN $ TE.decodeUtf8 $ JSON.encodePretty val
        liftIO $ print (val :: JSON.Value)
  lift $ logDebugN "Done loading page, getting logs"
  -- yieldMany logEntries
  liftIO $ threadDelay 10_000_000 -- TODO Temporary

decodeLogEntries :: ConduitT WD.LogEntry JSON.Value Import ()
decodeLogEntries = awaitForever $ \logEntry -> do
  -- There is a json value in the log entry's message
  -- we just try to decode it here.
  case decodeLogEntry logEntry of
    Left err -> pure ()
    -- logDebugN $
    --   T.pack $
    --     unlines
    --       [ unwords ["Failed to decode on the first round:", err],
    --         show contentsLB
    --       ]
    Right value -> do
      logDebugN $
        T.pack $
          unlines
            [ "Succesfully decoded this log message",
              T.unpack $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty value
            ]
      yield value

decodeLogEntry :: WD.LogEntry -> Either String JSON.Value
decodeLogEntry logEntry =
  let contentsLB = LB.fromStrict (TE.encodeUtf8 (logMsg logEntry))
   in JSON.eitherDecode contentsLB

decodeInterestingLogEntries :: ConduitT JSON.Value (JSON.Value, InterestingLog) Import ()
decodeInterestingLogEntries = awaitForever $ \value -> do
  case parseInterestingLogEntry value of
    Left err -> pure () -- logDebugN $ T.pack $ "Failed to parse interesting log: " <> err
    Right interestingLog -> yield (value, interestingLog)

parseInterestingLogEntry :: JSON.Value -> Either String InterestingLog
parseInterestingLogEntry = JSON.parseEither parseJSON

data InterestingLog = InterestingLog
  { interestingLogType :: !Text,
    interestingLogRequestId :: !Text,
    interestingLogScheme :: !String,
    interestingLogPath :: !String,
    interestingLogMethod :: !String,
    interestingLogAuthority :: !String,
    interestingLogAuthorization :: !Text,
    interestingLogContentType :: !Text,
    interestingLogAccept :: !Text,
    interestingLogAcceptEncoding :: !Text,
    interestingLogAcceptLanguage :: !Text,
    interestingLogAmzSecurityToken :: !Text,
    interestingLogAmzDate :: !Text,
    interestingLogAmzUserAgent :: !Text,
    interestingLogOrigin :: !Text,
    interestingLogReferer :: !Text,
    interestingLogUserAgent :: !Text
  }
  deriving (Show)

instance FromJSON InterestingLog where
  parseJSON = withObject "Outer" $ \o -> do
    message <- o .: "message"
    interestingLogType <- message .: "method"
    params <- message .: "params"
    interestingLogRequestId <- params .: "requestId"
    headers <- params .:? "headers"
    interestingLogScheme <- headers .: ":scheme"
    interestingLogPath <- headers .: ":path"
    interestingLogMethod <- headers .: ":method"
    interestingLogAuthority <- headers .: ":authority"
    interestingLogAuthorization <- headers .: "authorization"
    interestingLogContentType <- headers .: "content-type"
    interestingLogAccept <- headers .: "accept"
    interestingLogAcceptEncoding <- headers .: "accept-encoding"
    interestingLogAcceptLanguage <- headers .: "accept-language"
    interestingLogAmzSecurityToken <- headers .: "x-amz-security-token"
    interestingLogAmzDate <- headers .: "x-amz-date"
    interestingLogAmzUserAgent <- headers .: "x-amz-user-agent"
    interestingLogOrigin <- headers .: "origin"
    interestingLogReferer <- headers .: "referer"
    interestingLogUserAgent <- headers .: "user-agent"
    pure InterestingLog {..}

filterInterestingLogEntries :: ConduitT (JSON.Value, InterestingLog) Request Import ()
filterInterestingLogEntries = awaitForever $ \(value, il@InterestingLog {..}) -> do
  when ("requestWillBeSentExtraInfo" `T.isInfixOf` interestingLogType) $
    when ("graphql" `isInfixOf` interestingLogPath) $ do
      logDebugN $
        T.pack $
          unlines
            [ "JSON value value of the log message:",
              T.unpack $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty value
            ]
      case parseURI (interestingLogScheme <> "://" <> interestingLogAuthority <> interestingLogPath) of
        Nothing -> pure ()
        Just uri -> case requestFromURI uri of
          Nothing -> pure ()
          Just requestPrototype -> do
            logDebugN $ T.pack $ unlines ["Requesting with body", show queryRequestBody]
            let request =
                  requestPrototype
                    { requestHeaders =
                        [ ("X-Amz-Security-Token", TE.encodeUtf8 interestingLogAmzSecurityToken),
                          ("x-amz-date", TE.encodeUtf8 interestingLogAmzDate),
                          ("x-amz-user-agent", TE.encodeUtf8 interestingLogAmzUserAgent),
                          ("Referer", TE.encodeUtf8 interestingLogReferer),
                          ("Origin", TE.encodeUtf8 interestingLogOrigin),
                          ("Authorization", TE.encodeUtf8 interestingLogAuthorization),
                          ("Accept", TE.encodeUtf8 interestingLogAccept),
                          ("Accept-Encoding", TE.encodeUtf8 interestingLogAcceptEncoding),
                          ("Accept-Language", TE.encodeUtf8 interestingLogAcceptLanguage),
                          ("User-Agent", TE.encodeUtf8 interestingLogUserAgent),
                          ("Host", TE.encodeUtf8 (T.pack interestingLogAuthority)),
                          ("Content-Type", TE.encodeUtf8 interestingLogContentType),
                          ("Content-Length", "685"),
                          ("DNT", "1"),
                          ("Connection", "keep-alive")
                        ],
                      requestBody = RequestBodyLBS queryRequestBody
                    }
            liftIO $ pPrint $ requestHeaders request
            yield request

queryRequestBody :: LB.ByteString
queryRequestBody =
  JSON.encode $
    object
      [ "query" .= graphQlQuery,
        "variables"
          .= object
            [ "dateFrom" .= fromGregorian 2021 08 13,
              "limit" .= (42 :: Int),
              "eventType" .= ("party" :: Text)
            ]
      ]

graphQlQuery :: Text
graphQlQuery =
  [s|
  query EventsByType($eventType: String, $dateFrom: ModelStringKeyConditionInput, $sortDirection: ModelSortDirection, $filter: ModelEventFilterInput, $limit: Int, $nextToken: String) {
    eventsByType(eventType: $eventType, dateFrom: $dateFrom, sortDirection: $sortDirection, filter: $filter, limit: $limit, nextToken: $nextToken) {
      items {
        id
        region
        dateFrom
        shortDescription
        description
        image
        title
        eventType
        danceStyle
        location
        website
        owner
        createdAt
        updatedAt
      }
      nextToken
    }
  }
|]
