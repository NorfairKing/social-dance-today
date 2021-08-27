{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client as HTTP
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
    yield "https://www.latinworld.nl/salsa/agenda/"
      .| withPeriods
      .| makeAgendaRequestForPeriod
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseAgendaPageUrls
      .| deduplicateC
      .| makeEventPageRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| C.mapM_ (liftIO . pPrint)

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
parseAgendaPageUrls = awaitForever $ \(request, response) -> do
  let urls = fromMaybe [] $
        scrapeStringLike (responseBody response) $
          chroot "main" $
            chroot ("div" @: [hasClass "media"]) $
              chroot "table" $ do
                refs <- chroots "td" $ attr "href" "a"
                pure (mapMaybe maybeUtf8 refs :: [Text])
  yieldMany urls

makeEventPageRequest :: Monad m => ConduitT Text HTTP.Request m ()
makeEventPageRequest = C.concatMap $ \url -> parseRequest ("https://www.latinworld.nl/" <> T.unpack url) :: Maybe Request
