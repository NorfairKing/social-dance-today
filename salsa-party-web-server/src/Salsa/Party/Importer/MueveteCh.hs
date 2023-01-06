{-# LANGUAGE OverloadedStrings #-}

-- | https://muevete.ch
--
-- * There is a robots.txt that does not disallow anything.
-- * There is a terms of service but it doesn't mention crawling or search engine indexing
-- * There are jsonld events on the https://muevete.ch/salsaparties/ page.
--
-- This is the overal strategy:
--
-- 1. There are JSON LD events on this page, we just import them as-is: https://muevete.ch/salsaparties/
module Salsa.Party.Importer.MueveteCh (mueveteChImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import qualified Web.JSONLD as LD

mueveteChImporter :: Importer
mueveteChImporter =
  Importer
    { importerName = "muevete.ch",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = 1 -- Swiss time
    }

func :: Import ()
func =
  runConduit $
    yield "https://muevete.ch/salsaparties"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| jsonLDEventsC
      .| convertLDEventToExternalEventWith
        ( \_ ldEvent ->
            T.pack (show (LD.eventStartDate ldEvent)) <> fromMaybe "" (LD.eventUrl ldEvent)
        )
      .| C.mapM_ importExternalEventWithMImage
