{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://salsaOn2Happenings.ch
--
-- We've gotten permission from the owners to scrape what we want.
-- Let's do so, respectfully.
module Salsa.Party.Importer.SalsaOn2HappeningsCh (salsaOn2HappeningsChImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import Text.ICalendar.Parser as ICal

salsaOn2HappeningsChImporter :: Importer
salsaOn2HappeningsChImporter =
  Importer
    { importerName = "salsaOn2Happenings.ch",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    yield baseUri
      .| C.concatMap (\s -> requestFromURI s :: Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseCalendarUrls
      .| deduplicateC
      .| C.concatMap (\s -> requestFromURI s :: Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| C.map (\(req, resp) -> ICal.parseICalendar def (show (getUri req)) (responseBody resp))
      .| C.mapM_ (liftIO . print)

baseUri :: URI
baseUri = fromJust $ parseURI "https://salsaon2happenings.ch"

parseCalendarUrls :: Monad m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) URI m ()
parseCalendarUrls = awaitForever $ \(_, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          let links = mapMaybe maybeUtf8 refs
          pure $ map T.unpack $ filter (T.isSuffixOf ".ics") links
  yieldMany $ map (`URI.relativeTo` baseUri) $ mapMaybe parseURIReference uris
