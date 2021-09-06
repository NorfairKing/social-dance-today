{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://tanzagenda.ch
--
-- We've gotten permission from the owners to scrape what we want.
-- Let's do so, respectfully.
module Salsa.Party.Importer.TanzagendaCh (tanzagendaChImporter) where

import Conduit
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
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
    --    .| parseEventsLinks
    --    .| C.concatMap (requestFromURI :: URI -> Maybe Request)
    -- .|
    yield "https://tanzagenda.ch/events/Tanznacht40-Soho-231-234"
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseEventPage
      .| C.mapM_ (liftIO . pPrint)

withPages :: Monad m => ConduitT a (a, Int) m ()
withPages = awaitForever $ \a -> do
  yieldMany $ map ((,) a) [0 .. 4]

makeListRequest :: (String, Int) -> Maybe Request
makeListRequest (url, pageNum) = parseRequest $ url <> "&page=" <> show pageNum

parseEventsLinks :: Monad m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) URI m ()
parseEventsLinks = awaitForever $ \(request, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          let links = mapMaybe maybeUtf8 refs
          let eventsLinks = filter ("/events" `T.isPrefixOf`) links
          pure $ mapMaybe (parseURI . ("https://tanzagenda.ch" <>) . T.unpack) eventsLinks
  yieldMany uris
