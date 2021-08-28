{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://latinworld.nl
--
-- 1. There are no terms of services.
-- 2. There is an explicit copyright notice at https://www.latinworld.nl/over-latinworld.php but it metnions we're not allowed to reproduce anything.
--    However:
--    1. the robots.txt mentions that everyone has permission to crawl
--    2. Google reproduces things
-- 3. The robots.txt
--
-- All good so far, except the data is not machine readable and the sitemap.xml is incompelete.
module Salsa.Party.Importer.LatinworldNl (latinworldNlImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Safe
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

latinworldNlImporter :: Importer
latinworldNlImporter =
  Importer
    { importerName = "latinworld.nl",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    -- yield "https://www.latinworld.nl/salsa/agenda/"
    --   .| withPeriods
    --   .| makeAgendaRequestForPeriod
    --   .| doHttpRequestWith
    --   .| logRequestErrors
    --   .| parseAgendaPageUrls
    --   .| deduplicateC
    yield "latin/agenda/burn-the-floor-salsa-bachata-kizomba-29-08-2021-salsadansschool-mambomike-rotterdam-83867.php"
      .| makeEventPageRequest
      .| doHttpRequestWith'
      .| logRequestErrors'
      .| importEventPage

withPeriods :: Monad m => ConduitT a (a, Maybe Int) m ()
withPeriods = awaitForever $ \a -> yieldMany $ map ((,) a) [Nothing, Just 1, Just 2, Just 3, Just 4]

makeAgendaRequestForPeriod :: Monad m => ConduitT (String, Maybe Int) HTTP.Request m ()
makeAgendaRequestForPeriod = C.concatMap $ \(url, mp) ->
  parseRequest
    ( case mp of
        Nothing -> url
        Just p -> url <> "?periode=" <> show p
    ) ::
    Maybe Request

parseAgendaPageUrls :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Text Import ()
parseAgendaPageUrls = awaitForever $ \(_, response) -> do
  let urls = fromMaybe [] $
        scrapeStringLike (responseBody response) $
          chroot "main" $
            chroot ("div" @: [hasClass "media"]) $
              chroot "table" $ do
                refs <- chroots "td" $ attr "href" "a"
                pure (mapMaybe maybeUtf8 refs :: [Text])
  yieldMany urls

makeEventPageRequest :: Monad m => ConduitT Text (Text, HTTP.Request) m ()
makeEventPageRequest = C.concatMap $ \url ->
  (,) url <$> (parseRequest ("https://www.latinworld.nl/" <> T.unpack url) :: Maybe Request)

importEventPage :: ConduitT (Text, HTTP.Request, HTTP.Response LB.ByteString) Void Import ()
importEventPage = awaitForever $ \(relativeUrl, request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
      yesterday = addDays (-1) today
  let eventScraper :: ScraperT LB.ByteString Import (ExternalEvent, Maybe Text)
      eventScraper = chroot "main" $
        chroot ("div" @: [hasClass "row"]) $ do
          externalEventUuid <- nextRandomUUID
          let externalEventKey = relativeUrl

          let decodeLenient = T.strip . TE.decodeUtf8With TE.lenientDecode . LB.toStrict

          header <- decodeLenient <$> text "h1"
          (dateText, titleText) <- case T.splitOn ": " header of
            (dateText : rest) -> do
              let replaceSpace = \case
                    '\65533' -> ' '
                    c -> c
              pure
                ( T.unwords $ drop 1 $ T.words $ T.map replaceSpace dateText,
                  T.intercalate ": " rest
                )
            _ -> fail "Failed to decode the header"

          let dayTextForParsing = ' ' : filter (\c -> not (Char.isSpace c) && (c /= '\65533')) (T.unpack dateText)
          day <- case parseTimeM False dutchTimeLocale "%e%b%Y" dayTextForParsing of
            Nothing -> fail "Could not parse day"
            Just d -> pure d

          let externalEventTitle = titleText

          guard $ day >= yesterday
          let externalEventDay = day

          mTrip <- fmap listToMaybe $
            chroots "table" $ do
              h4 <- text "h4"
              guard $ h4 == "Locatie"
              cells <- texts "td"
              let cell ix = case atMay cells ix of
                    Nothing -> fail "cell not found"
                    Just res -> pure res
              mOrganiser <- optional $ decodeLenient <$> cell 1
              addr1 <- decodeLenient <$> cell 16
              addr2 <- decodeLenient <$> cell 19
              addr3 <- decodeLenient <$> cell 22
              mRawLink <- optional $ decodeLenient <$> cell 24
              let address = T.intercalate ", " [addr1, addr2, addr3]
              let mLink = mRawLink >>= (headMay . dropWhile T.null . map T.strip . T.words)
              pure (address, mOrganiser, mLink)

          (address, externalEventOrganiser, externalEventHomepage) <- case mTrip of
            Nothing -> fail "no address"
            Just (a, mO, mL) -> pure (a, mO, mL)

          app <- asks importEnvApp
          mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
          externalEventPlace <- case mPlaceEntity of
            Nothing -> fail "could not geolocate"
            Just (Entity placeId _) -> pure placeId

          (externalEventStart, externalEventPrice) <- fmap (fromMaybe (Nothing, Nothing) . listToMaybe) $
            chroots ("table" @: ["style" @= "width: 100%"]) $ do
              cells <- texts "td"
              let cell ix = case atMay cells ix of
                    Nothing -> fail "cell not found"
                    Just res -> pure res
              rawDateCell <- cell 2
              guard $ "datum" `T.isInfixOf` decodeLenient rawDateCell
              rawStart <- fmap join $ optional $ headMay . T.words . decodeLenient <$> cell 5
              let mStart = rawStart >>= (parseTimeM True dutchTimeLocale "%H:%M" . T.unpack)
              mPrice <- optional $ T.strip . decodeLenient <$> cell 7
              pure (mStart, mPrice)

          cancelled <- fmap (fromMaybe False) $
            optional $ do
              t <- text ("font" @: ["color" @= "red"])
              pure $ "geannuleerd" `T.isInfixOf` decodeLenient t

          let externalEventDescription = Nothing
          let externalEventCancelled = cancelled
          let externalEventCreated = now
          let externalEventModified = Nothing
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request

          mImageLink <- fmap join $
            optional $ do
              refs <- attrs "src" "img"
              let candidates = mapMaybe maybeUtf8 refs
              pure $ find ("/media/flyers" `T.isPrefixOf`) candidates

          pure (ExternalEvent {..}, mImageLink)
  lift $ do
    mExternalEvent <- scrapeStringLikeT (responseBody response) eventScraper
    forM_ mExternalEvent $ \(externalEvent, mImageUrl) -> do
      importExternalEventAnd externalEvent $ \externalEventId -> do
        case mImageUrl >>= (parseURI . ("https://www.latinworld.nl/" <>) . T.unpack) of
          Nothing -> pure ()
          Just uri -> do
            mImageId <- tryToImportImage uri
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
