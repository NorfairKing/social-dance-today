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
--
-- This importer uses selenium to download the page and get the graphql json response from the request logs.
--
-- Note that the selenium server will remove old data if the disk is getting full.
-- In that case we won't find anything using this importer.
module Salsa.Party.Importer.SensualDance (sensualDanceImporter) where

import Conduit
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.List
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
        .| importItem

fetchHome :: ConduitT String (Item, Maybe ByteString) Import ()
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
      let responseLogs = rights $ map parseResponseLog logValues
      -- For some reason every url shows up twice.
      -- once with a request id like F20757673B3A0F3F215394EC96C8879B, once with a request id like 2705.29.
      -- We only want the later, so we use M.fromList to throw away the first of every duplicate.
      let responseLogMap = M.fromList $ map (\ResponseLog {..} -> (responseLogUrl, responseLogRequestId)) responseLogs

      -- First we get all graphql requests out of here to find the text-based information that we can; the items
      let graphqlRequestMap = M.filterWithKey (\k _ -> "/graphql" `T.isSuffixOf` k) responseLogMap

      items <- fmap concat $
        forM graphqlRequestMap $ \requestId -> do
          GetResponseBodyResponse {..} <- getResponseBodyFor requestId
          pure $
            if getResponseBodyResponseBase64Encoded
              then []
              else case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 getResponseBodyResponseBody)) of
                Left _ -> []
                Right resp -> graphQLResponseItems resp

      -- Next, we try to find the image for each event in the request logs as well
      let imageRequestMap = M.filterWithKey (\k _ -> "sensualdance-images" `T.isInfixOf` k) responseLogMap

      forM items $ \item -> do
        mImage <- fmap join $
          forM (itemImage item) $ \imageId ->
            case find (\(k, _) -> imageId `T.isInfixOf` k) (M.toList imageRequestMap) of
              Nothing -> pure Nothing
              Just (_, requestId) -> do
                GetResponseBodyResponse {..} <- getResponseBodyFor requestId
                pure $
                  if getResponseBodyResponseBase64Encoded
                    then case Base64.decode (TE.encodeUtf8 getResponseBodyResponseBody) of
                      Left _ -> Nothing
                      Right bs -> Just bs
                    else Nothing
        pure (item, mImage)

  lift $ logDebugN "Done loading page, getting logs"
  yieldMany vals

getResponseBodyFor :: Text -> WD GetResponseBodyResponse
getResponseBodyFor requestId = do
  let arg :: JSON.Value
      arg =
        object
          [ "cmd" .= ("Network.getResponseBody" :: Text),
            "params"
              .= object
                [ "requestId" .= requestId
                ]
          ]
  doSessCommand methodPost "/goog/cdp/execute" arg

decodeLogEntry :: WD.LogEntry -> Either String JSON.Value
decodeLogEntry logEntry =
  let contentsLB = LB.fromStrict (TE.encodeUtf8 (logMsg logEntry))
   in JSON.eitherDecode contentsLB

parseResponseLog :: JSON.Value -> Either String ResponseLog
parseResponseLog = JSON.parseEither parseJSON

data ResponseLog = ResponseLog
  { responseLogRequestId :: !Text,
    responseLogUrl :: !Text
  }
  deriving (Show)

instance FromJSON ResponseLog where
  parseJSON = withObject "ResponseLog" $ \o -> do
    message <- o .: "message"
    method <- message .: "method"
    guard $ method == ("Network.responseReceived" :: Text)
    params <- message .: "params"
    responseLogRequestId <- params .: "requestId"
    response <- params .: "response"
    responseLogUrl <- response .: "url"
    pure ResponseLog {..}

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
    itemDateFrom :: !ZonedTime,
    itemImage :: !(Maybe Text)
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
    itemImage <- o .:? "image"
    pure Item {..}

importItem :: ConduitT (Item, Maybe ByteString) void Import ()
importItem = awaitForever $ \(Item {..}, mImageBlob) -> do
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

              lift $
                importExternalEventAnd ExternalEvent {..} $ \externalEventId -> do
                  forM_ mImageBlob $ \imageBlob -> do
                    mImageId <- tryToImportImageBlob "image/jpeg" imageBlob
                    forM_ mImageId $ \imageId -> do
                      importDB $
                        upsertBy
                          (UniqueExternalEventPoster externalEventId)
                          ( ExternalEventPoster
                              { externalEventPosterExternalEvent = externalEventId,
                                externalEventPosterImage = imageId,
                                externalEventPosterCreated = now,
                                externalEventPosterModified = Nothing
                              }
                          )
                          [ ExternalEventPosterImage =. imageId,
                            ExternalEventPosterModified =. Just now
                          ]

regionMap :: Map Text Text
regionMap =
  M.fromList
    [ ("ch-ost", "Zurich"),
      ("ch-bsnw", "Basel"),
      ("ch-bern", "Bern")
    ]
