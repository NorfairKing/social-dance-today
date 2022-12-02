{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Constants where

import Language.Haskell.TH
import System.Environment

development :: Bool
development =
  $( do
       md <- runIO $ lookupEnv "DEVELOPMENT"
       fmap ConE $ case md of
         Nothing -> pure 'False
         Just _ -> do
           runIO $ putStrLn "WARNING: BUILDING IN DEVELOPMENT MODE"
           pure 'True
   )

-- | How many days to show in search results
defaultDaysAhead :: Integer
defaultDaysAhead = 7

-- | How big of a search range we allow
maximumSearchRange :: Integer
maximumSearchRange = 5 * defaultDaysAhead

-- | How many days to keep parties in the database before they can be
-- garbage-collected
--
-- We want to delete old (external) events eventually because they fill up our
-- database and slow down queries.
-- However, we can't delete them too quickly because google will still try to
-- link to them.
-- We used to use 30 here, but it turned out to not be enough, so now we're
-- trying 90 instead
daysToKeepParties :: Integer
daysToKeepParties = 90

-- | How many days to mark we keep parties for
--
-- We tag pages with 'unavailable_after' earlier than
-- 'daysToKeepParties' to prevent further 404s and/or 410s.
daysToKeepPartiesMarkedAsAvailable :: Integer
daysToKeepPartiesMarkedAsAvailable = 30
