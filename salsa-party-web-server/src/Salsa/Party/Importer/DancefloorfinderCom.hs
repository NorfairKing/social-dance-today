{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://dancefloorfinder.com
--
-- 1. There are no terms of service.
-- 2. The robots.txt does not forbid crawling.
--
-- This is a SPA, with a JSON API, extra easy!
--
-- Under https://dancefloorfinder.com/api/home, there is a list of cities.
--
-- Under https://dancefloorfinder.com/api/:city, there is a list of parties in this format:
--
--   {
--     "id": 0,
--     "date": "2021-09-12",
--     "title": "Oslo Cuban Salsa Bootcamp",
--     "dances": [
--       "salsa"
--     ],
--     "start_at": "11:00",
--     "end_at": "15:00",
--     "country": "Norway",
--     "city": "Oslo",
--     "price": "490",
--     "address": "Dancify Studio Eikenga 11, 0579 Oslo, Norwa",
--     "link": "https://www.facebook.com/events/4133405723372615",
--     "organization": null
--   }
--
--  Easy peasy.
--
--  Most of these contain a facebook event link, so not handy, but fine for now.
--
--  We'll put the "dances" list in the description but otherwise keep the description clean.
module Salsa.Party.Importer.DancefloorfinderCom (dancefloorfinderComImporter) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Importer.TribeCalendar

dancefloorfinderComImporter :: Importer
dancefloorfinderComImporter =
  Importer
    { importerName = "dancefloorfinder.com",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "http://dancefloorfinder.com/api/home"
      .| C.mapM_ (liftIO . pPrint)
