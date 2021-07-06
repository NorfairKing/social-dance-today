{-# LANGUAGE OverloadedStrings #-}

-- | https://www.danceus.org/
--
-- The terms of service mention that we're not allowed to scrape their site, as does their robots.txt.
--
-- Details anyway:
--
-- Home: https://www.danceus.org/
--
-- On the https://www.danceus.org/events/ page, there are event calendars per dance style and per location.
--
-- They are in `<a class="calendar-link" href="[...]">` tags.
-- From there, there are pages like:
--
-- https://www.danceus.org/events/salsa/aachen-germany-salsa-calendar/
--
-- On those pages, there are events inside tags like these:
--
-- <a class="item-title " href="/event/15830025762890/salsa-after-work-apollo-aachen-germany/">
--
-- Clicking "more events" also shows a page with an url like this:
-- https://www.danceus.org/search-events/salsa/?q=Aachen,%20Germany&lat=50.775429&lng=6.08149&rad=30&order=distance
-- So we may be able to do some searches as well, great!
--
-- Example event page:
-- https://www.danceus.org/event/15830025762890/salsa-after-work-apollo-aachen-germany/
--
-- On each event page, there is a lot of information, but I don't think there is a way to see them in JSON format.
-- Luckily though, the <head> tag contains a JSONLD tag:
-- <script type="application/ld+json">
-- And this contains a "@type": "Event", which we can import.
module Salsa.Party.Importer.DanceUsOrg (danceUsOrgImporter) where

import Conduit
import Data.Aeson as JSON
import qualified Data.Conduit.Combinators as C
import Data.Fixed
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI as URI
import Salsa.Party.Importer.Import

danceUsOrgImporter :: Importer
danceUsOrgImporter =
  Importer
    { importerName = "danceus.org",
      importerFunc = func
    }

func :: Import ()
func = do
  pure ()
