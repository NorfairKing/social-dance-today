{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://tanzagenda.ch
--
-- We've gotten permission from the owners to scrape what we want.
-- Let's do so, respectfully.
module Salsa.Party.Importer.TanzagendaCh (tanzagendaChImporter) where

import Conduit
import Control.Applicative
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

tanzagendaChImporter :: Importer
tanzagendaChImporter =
  Importer
    { importerName = "tanzagenda.ch",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    --  yield "https://www.tanzagenda.ch/_info/customDataLoader/eventsData.php?get=data"
    --    .| withPages
    --    .| C.concatMap makeListRequest
    --    .| doHttpRequestWith
    --    .| logRequestErrors
    --    .| parseEventsKeys
    -- .|
    yield "Tanznacht40-Soho-231-234"
      .| C.concatMap (\k -> (,) k <$> parseRequest ("https://tanzagenda.ch/events/" <> T.unpack k) :: Maybe (Text, Request))
      .| doHttpRequestWith'
      .| logRequestErrors'
      .| parseEventPage
      .| C.mapM_ (liftIO . pPrint)

withPages :: Monad m => ConduitT a (a, Int) m ()
withPages = awaitForever $ \a -> do
  yieldMany $ map ((,) a) [0 .. 4]

makeListRequest :: (String, Int) -> Maybe Request
makeListRequest (url, pageNum) = parseRequest $ url <> "&page=" <> show pageNum

parseEventsKeys :: Monad m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Text m ()
parseEventsKeys = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          let links = mapMaybe maybeUtf8 refs
          pure $ mapMaybe (T.stripPrefix "/events/") links
  yieldMany uris

parseEventPage :: ConduitT (Text, HTTP.Request, HTTP.Response LB.ByteString) Void Import ()
parseEventPage = awaitForever $ \(key, request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let scrapeExternalEventFromPage :: ScraperT LB.ByteString Import (ExternalEvent, Maybe URI)
      scrapeExternalEventFromPage = chroot ("section" @: ["id" @= "events"]) $ do
        externalEventUuid <- nextRandomUUID

        let externalEventKey = key

        rawTitle <- text "h1"
        liftIO $ pPrint ("rawTitle", rawTitle)
        externalEventTitle <- utf8 rawTitle

        chroot ("div" @: [hasClass "card-body"]) $ do
          externalEventDay <- chroot ("div" @: [hasClass "row"]) $
            chroot ("div" @: [hasClass "col"]) $ do
              rawDay <- text "h2"
              dayText <- utf8 rawDay
              day <- case parseTimeM True germanTimeLocale "%a %d. %B %Y" (T.unpack dayText) of
                Nothing -> fail "day not parseable"
                Just d -> pure d
              guard $ day >= addDays (-1) today
              pure day

          externalEventPlace <- chroot ("div" @: [hasClass "row"]) $
            chroot ("div" @: [hasClass "col"]) $
              chroot ("div" @: [hasClass "row"]) $ do
                rawAddress <- text ("div" @: ["class" @= "row"])
                addressText <- utf8 rawAddress
                let strippedAddress = T.strip addressText
                case T.stripSuffix "Mehr über das Lokal" strippedAddress of
                  Nothing -> fail "not the right piece"
                  Just withoutSuffix -> do
                    let address = T.unwords $ filter (not . T.null) $ map T.strip $ T.words withoutSuffix

                    app <- asks importEnvApp
                    mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
                    case mPlaceEntity of
                      Nothing -> fail "could not geolocate"
                      Just (Entity placeId _) -> pure placeId

          let externalEventStart = Nothing

          externalEventDescription <- optional $
            chroot ("div" @: ["class" @= "col-12"]) $ do
              rawSentences <- texts "p"
              liftIO $ pPrint ("rawSentences", rawSentences)
              let sentences = map (T.strip . TE.decodeLatin1 . LB.toStrict) rawSentences
              liftIO $ pPrint ("sentences", sentences)
              pure $ T.unlines sentences
          liftIO $ pPrint ("externalEventDescription", externalEventDescription)

          let externalEventOrganiser = Nothing -- TODO
          let externalEventCancelled = False -- TODO
          let externalEventHomepage = Nothing -- TODO
          let externalEventPrice = Nothing

          let externalEventCreated = now
          let externalEventModified = Nothing
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request

          let mImageUri = Nothing -- TODO
          pure (ExternalEvent {..}, mImageUri)
  lift $ do
    mTup <- scrapeStringLikeT (responseBody response) scrapeExternalEventFromPage
    mapM_ importExternalEventWithMImage mTup
