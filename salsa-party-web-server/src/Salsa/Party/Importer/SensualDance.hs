{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- We need this for the WD.chrome piece
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-- | https://sensual.dance
--
-- 1. There are no terms of service.
-- 2. The robots.txt does not forbid crawling.
--
-- This page is a real nightmare.
-- It only queries the parties at runtime, via javascript, using a graphql client that uses authentication.
module Salsa.Party.Importer.SensualDance (sensualDanceImporter) where

import Conduit
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Salsa.Party.Importer.Import
import Salsa.Party.Importer.Selenium
import Salsa.Party.Web.Server.Geocoding
import Test.WebDriver as WD
import Test.WebDriver.Class as WD
import Test.WebDriver.Commands.Internal as WD
import Test.WebDriver.Commands.Wait as WD

sensualDanceImporter :: Importer
sensualDanceImporter =
  Importer
    { importerName = "sensual.dance",
      importerFunc = func
    }

func :: Import ()
func =
  withSeleniumServer $
    runConduit $
      yield "http://sensual.dance/"
        .| fetchHome
        .| getResponseBodyValues
        .| importItem

fetchHome :: ConduitT String GetResponseBodyResponse Import ()
fetchHome = awaitForever $ \homeUrl -> do
  lift $ waitToFetch homeUrl
  lift $ logDebugN $ T.pack $ "Fetching: " <> homeUrl
  chromeExecutable <- getChromeExecutable
  vals <- liftIO $ do
    let browser =
          WD.chrome
            { chromeOptions = ["--headless", "--no-sandbox"],
              chromeBinary = Just $ fromAbsFile chromeExecutable
            }
        caps =
          WD.defaultCaps
            { browser = browser,
              javascriptEnabled = Just True,
              additionalCaps =
                [ ( "goog:loggingPrefs",
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
      WD.setImplicitWait 10_000 -- Ten seconds
      WD.setScriptTimeout 10_000 -- Ten seconds
      WD.setPageLoadTimeout 10_000 -- Ten seconds
      WD.openPage homeUrl
      _ <- WD.waitUntil 10 $ do
        t <- executeJS [] "return document.readyState"
        WD.expect $ (t :: Text) == "complete"
      liftIO $ threadDelay 10_000_000 -- Wait for the entire page to be loaded.
      logEntries <- getLogs "performance"
      let logValues = rights $ map decodeLogEntry logEntries
      let interestingLogEntries = rights $ map parseInterestingLogEntry logValues
      forM interestingLogEntries $ \InterestingLog {..} -> do
        let arg :: JSON.Value
            arg =
              object
                [ "cmd" .= ("Network.getResponseBody" :: Text),
                  "params"
                    .= object
                      [ "requestId" .= interestingLogRequestId
                      ]
                ]
        doSessCommand methodPost "/goog/cdp/execute" arg

  lift $ logDebugN "Done loading page, getting logs"
  yieldMany vals

decodeLogEntry :: WD.LogEntry -> Either String JSON.Value
decodeLogEntry logEntry =
  let contentsLB = LB.fromStrict (TE.encodeUtf8 (logMsg logEntry))
   in JSON.eitherDecode contentsLB

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
    headers <- params .: "headers"
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

data GetResponseBodyResponse = GetResponseBodyResponse
  { getResponseBodyResponseBody :: !Text,
    getResponseBodyResponseBase64Encoded :: !Bool
  }
  deriving (Show, Eq)

instance FromJSON GetResponseBodyResponse where
  parseJSON = withObject "GetResponseBodyResponse" $ \o ->
    GetResponseBodyResponse
      <$> o .: "body"
      <*> o .: "base64Encoded"

getResponseBodyValues :: ConduitT GetResponseBodyResponse Item Import ()
getResponseBodyValues = awaitForever $ \GetResponseBodyResponse {..} -> do
  if getResponseBodyResponseBase64Encoded
    then logDebugN "Found a base64 encoded response body, ignoring it."
    else case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 getResponseBodyResponseBody)) of
      Left err -> logDebugN $ T.pack $ "Failed to decode: " <> err
      Right value -> do
        logDebugN $
          T.pack $
            unlines
              [ "Found this json value in the response body:",
                T.unpack $ TE.decodeUtf8 (LB.toStrict (JSON.encodePretty value))
              ]
        case JSON.parseEither parseJSON value of
          Left err -> logDebugN $ T.pack $ "Failed to decode graphql response: " <> err
          Right resp -> yieldMany $ graphQLResponseItems resp

data GraphQLResponse = GraphQLResponse
  { graphQLResponseItems :: [Item]
  }
  deriving (Show)

instance FromJSON GraphQLResponse where
  parseJSON = withObject "GraphQLResponse" $ \o -> do
    dat <- o .: "data"
    eventsByType <- dat .: "eventsByType"
    GraphQLResponse <$> eventsByType .: "items"

-- For unknown reasons, 'location' is an empty text? Maybe we need to revisit this as the website develops.
-- The 'region' category is the closest we get to something useful, so we try to use that for now.
data Item = Item
  { itemId :: !Text,
    itemTitle :: !Text,
    itemRegion :: !Text,
    itemDescription :: !(Maybe Text),
    itemWebsite :: !(Maybe Text),
    itemDateFrom :: !ZonedTime
  }
  deriving (Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    itemId <- o .: "id"
    itemTitle <- o .: "title"
    itemRegion <- o .: "region"
    -- Description is encoded at the key 'description',
    -- as a text with is a json object, so double-encoded.
    -- In there, there is a list of blocks which each contain a 'text'.
    -- What a mess.
    mDescriptionText <- o .:? "description"
    let mDescriptionVal =
          mDescriptionText >>= \descriptionText -> case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 descriptionText)) of
            Left _ -> Nothing
            Right v -> Just v
    itemDescription <- forM (mDescriptionVal :: Maybe JSON.Value) $
      withObject "DescriptionVal" $ \d -> do
        blocks <- d .: "blocks"
        fmap T.unlines $ forM blocks $ withObject "Block" $ \b -> b .: "text"
    itemWebsite <- o .:? "website"
    itemDateFrom <- o .: "dateFrom"
    pure Item {..}

importItem :: ConduitT Item void Import ()
importItem = awaitForever $ \Item {..} -> do
  now <- liftIO getCurrentTime
  let today = utctDay now

  externalEventUuid <- nextRandomUUID
  let externalEventKey = itemId

  let externalEventTitle = itemTitle
  let externalEventDescription = itemDescription
  let externalEventOrganiser = Nothing
  let localTime = zonedTimeToLocalTime itemDateFrom
  let externalEventDay = localDay localTime
  if externalEventDay < addDays (-1) today
    then pure ()
    else do
      let externalEventStart =
            let tod = localTimeOfDay localTime
             in if tod /= midnight then Just tod else Nothing
      let externalEventHomepage = itemWebsite
      let externalEventPrice = Nothing
      let externalEventCancelled = False
      case M.lookup itemRegion regionMap of
        Nothing -> logDebugN $ "Could not categorise region: " <> itemRegion
        Just address -> do
          app <- asks importEnvApp
          mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
          case mPlaceEntity of
            Nothing -> pure ()
            Just (Entity externalEventPlace _) -> do
              let externalEventCreated = now
              let externalEventModified = Nothing
              externalEventImporter <- asks importEnvId
              let externalEventOrigin = "https://sensual.dance/event/id=" <> itemId

              lift $ importExternalEvent ExternalEvent {..}

regionMap :: Map Text Text
regionMap =
  M.fromList
    [ ("ch-ost", "Zurich"),
      ("ch-bsnw", "Basel"),
      ("ch-bern", "Bern")
    ]
