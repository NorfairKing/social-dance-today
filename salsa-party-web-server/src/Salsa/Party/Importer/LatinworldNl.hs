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
--
-- We import what we can based on the html, but the html doesn't exactly help.
-- It's table-based layout with no metadata for the most part.
--
-- TODO import the description as well.
-- As a human it's easy to see which part is the description, but in the HTML
-- it's just a <div> and that doesn't help at all :(
module Salsa.Party.Importer.LatinworldNl (latinworldNlImporter) where

import Conduit
import Control.Applicative
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Safe
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

latinworldNlImporter :: Importer
latinworldNlImporter =
  Importer
    { importerName = "latinworld.nl",
      importerFunc = func,
      importerUserAgent = UserAgentSocial,
      importerTimezoneOffset = 1 -- Dutch timezone
    }

func :: Import ()
func = do
  runConduit $
    yieldManyShuffled
      [ "https://www.latinworld.nl/salsa/agenda/",
        "https://www.latinworld.nl/bachata/agenda/",
        "https://www.latinworld.nl/kizomba/agenda/",
        "https://www.latinworld.nl/cubaanse-salsa/agenda/"
      ]
      .| withPeriods
      .| makeAgendaRequestForPeriod
      .| httpRequestC
      .| httpBodyTextParserC
      .| parseAgendaPageUrls
      .| deduplicateC
      .| makeEventPageRequest
      .| httpRequestC'
      .| httpBodyTextParserC'
      .| importEventPage

withPeriods :: MonadIO m => ConduitT a (a, Maybe Int) m ()
withPeriods = awaitForever $ \a -> yieldManyShuffled $ map ((,) a) [Nothing, Just 1, Just 2, Just 3, Just 4]

makeAgendaRequestForPeriod :: Monad m => ConduitT (String, Maybe Int) HTTP.Request m ()
makeAgendaRequestForPeriod = C.concatMap $ \(url, mp) ->
  parseRequest
    ( case mp of
        Nothing -> url
        Just p -> url <> "?periode=" <> show p
    ) ::
    Maybe Request

parseAgendaPageUrls :: ConduitT (HTTP.Request, HTTP.Response Text) Text Import ()
parseAgendaPageUrls = awaitForever $ \(_, response) -> do
  let urls =
        fromMaybe [] $
          scrapeStringLike (responseBody response) $
            chroot "main" $
              chroot ("div" @: [hasClass "media"]) $
                chroot "table" $
                  chroots "td" $ attr "href" "a"
  yieldManyShuffled urls

makeEventPageRequest :: Monad m => ConduitT Text (Text, HTTP.Request) m ()
makeEventPageRequest = C.concatMap $ \url ->
  (,) url <$> (parseRequest ("https://www.latinworld.nl/" <> T.unpack url) :: Maybe Request)

importEventPage :: ConduitT (Text, (HTTP.Request, HTTP.Response Text)) Void Import ()
importEventPage = awaitForever $ \(relativeUrl, (request, response)) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
      yesterday = addDays (-1) today
  let eventScraper :: ScraperT Text Import (ExternalEvent, Maybe URI)
      eventScraper = chroot "main" $
        chroot ("div" @: [hasClass "row"]) $ do
          externalEventUuid <- nextRandomUUID

          let externalEventKey = relativeUrl

          header <- text "h1"
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
          let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

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
              mOrganiser <- optional $ cell 1
              rawAddressCell <- cell 15
              guard $ "adres" `T.isInfixOf` rawAddressCell
              addr1 <- T.strip <$> cell 16
              addr2 <- T.strip <$> cell 19
              addr3 <- T.strip <$> cell 22
              mRawLink <- optional $ do
                rawLinkCell <- cell 23
                guard $ "zie ook" `T.isInfixOf` rawLinkCell
                cell 24
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
              guard $ "datum" `T.isInfixOf` rawDateCell
              rawStart <- fmap join $
                optional $ do
                  rawTimeCell <- cell 4
                  guard $ "tijd" `T.isInfixOf` rawTimeCell
                  headMay . T.words <$> cell 5
              let mStart = rawStart >>= (parseTimeM True dutchTimeLocale "%H:%M" . T.unpack)
              mPrice <- optional $ T.strip <$> cell 7
              pure (mStart, mPrice)

          externalEventCancelled <-
            optional $ do
              t <- text ("font" @: ["color" @= "red"])
              pure $ "geannuleerd" `T.isInfixOf` t

          let externalEventDescription = Nothing

          let externalEventPoster = Nothing

          let externalEventCreated = now
          let externalEventModified = Nothing
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request

          mImageUri <- fmap join $
            optional $ do
              refs <- attrs "src" "img"
              pure $ find ("/media/flyers" `T.isPrefixOf`) refs >>= parseURI . ("https://www.latinworld.nl/" <>) . T.unpack

          pure (ExternalEvent {..}, mImageUri)
  let prospectScraper :: Entity ExternalEvent -> ScraperT Text Import (Maybe Prospect)
      prospectScraper (Entity externalEventId ExternalEvent {..}) = fmap listToMaybe $
        chroots ("table" @: ["style" @= "width: 100%"]) $ do
          cells <- texts "td"
          let cell ix = case atMay cells ix of
                Nothing -> fail "cell not found"
                Just res -> pure res

          prospectName <- cell 1

          emailLabel <- cell 6
          guard $ "email" `T.isInfixOf` emailLabel
          prospectEmailAddress <- cell 7

          -- The organiser and the event are on the same page and there is only one address
          let prospectPlace = Just externalEventPlace

          let prospectExternalEvent = Just externalEventId

          let prospectCreated = now
          let prospectModified = Nothing
          prospectSecret <- nextRandomUUID
          let prospectUnsubscribed = Nothing
          let prospectInvited = Nothing
          pure Prospect {..}

  lift $ do
    mTup <- scrapeStringLikeT (responseBody response) eventScraper
    forM mTup $ \(externalEvent, mImageUri) -> do
      mExternalEventId <- importExternalEventWithMImage (externalEvent, mImageUri)
      forM mExternalEventId $ \externalEventId -> do
        mmProspect <- scrapeStringLikeT (responseBody response) (prospectScraper (Entity externalEventId externalEvent))
        mapM_ importProspect (join mmProspect)
