{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main as Criterion
import Data.GenValidity
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.Syd

main :: IO ()
main = unSetupFunc salsaConnectionPoolSetupFunc $ \pool ->
  Criterion.defaultMain
    [ env
        (setupSearchData pool)
        (doSearchAtBenchmark pool)
    ]

runGen :: Gen a -> a
runGen (MkGen func) = func qcGen 30
  where
    qcGen = mkQCGen 42

setupSearchData :: ConnectionPool -> IO [SearchQuery]
setupSearchData = runSqlPool setupSearchDataQuery

setupSearchDataQuery :: SqlPersistT IO [SearchQuery]
setupSearchDataQuery = do
  let places = runGen $
        genUniques placeQuery 5000 $ do
          location <- elements locations
          genPlaceAroundLocation (locationPlace location)
  placeIds <- insertMany places

  let (partyPrototypes, externalEventPrototypes) = runGen $ genPrototypes placeIds

  -- Set up parties
  partyIds <- forM partyPrototypes $ \partyPrototype -> do
    uuid <- nextRandomUUID
    insert $ partyPrototype {partyUuid = uuid}
  -- Set up external events
  externalEventIds <- forM externalEventPrototypes $ \externalEventPrototype -> do
    uuid <- nextRandomUUID
    insert $ externalEventPrototype {externalEventUuid = uuid}

  pure $ runGen $ replicateM 100 genQuery

genQuery :: Gen SearchQuery
genQuery = do
  -- Make a query
  day <- genDay
  daysAhead <- choose (0, 30)
  coordinates <- do
    location <- elements locations
    genCoordinatesAround (placeCoordinates (locationPlace location))
  distance <- choose (minimumMaximumDistance, maximumMaximumDistance)
  pure
    SearchQuery
      { searchQueryBegin = day,
        searchQueryMEnd = Just $ addDays daysAhead day,
        searchQueryCoordinates = coordinates,
        searchQueryDistance = Just distance
      }

genPrototypes :: [PlaceId] -> Gen ([Party], [ExternalEvent])
genPrototypes placeIds = (,) <$> genPartyPrototypes placeIds <*> genExternalEventPrototypes placeIds

genPartyPrototypes :: [PlaceId] -> Gen [Party]
genPartyPrototypes placeIds = replicateM 1000 $ do
  placeId <- elements placeIds
  day <- genDay
  partyPrototype <- genValid
  pure $
    partyPrototype
      { partyDay = day,
        partyPlace = placeId
      }

genExternalEventPrototypes :: [PlaceId] -> Gen [ExternalEvent]
genExternalEventPrototypes placeIds = do
  genUniques (\ExternalEvent {..} -> (externalEventImporter, externalEventKey)) 1000 $ do
    placeId <- elements placeIds
    day <- genDay
    externalEventPrototype <- genValid
    pure $
      externalEventPrototype
        { externalEventDay = day,
          externalEventPlace = placeId
        }

genUniques :: Eq b => (a -> b) -> Word -> Gen a -> Gen [a]
genUniques func n gen = go [] n
  where
    go acc i
      | i <= 0 = pure acc
      | otherwise = do
        a <- gen
        if func a `elem` map func acc
          then go acc i
          else go (a : acc) (pred i)

-- We generate days within a year, there's no point in looking accross centuries.
genDay :: Gen Day
genDay = fromGregorian 2021 <$> choose (1, 12) <*> choose (1, 31)

doSearchAtBenchmark :: ConnectionPool -> [SearchQuery] -> Benchmark
doSearchAtBenchmark pool query = bench "search" $ whnfIO $ doSearchAt pool query

doSearchAt :: ConnectionPool -> [SearchQuery] -> IO [SearchResult]
doSearchAt pool queries = runSqlPool (mapM runSearchQuery queries) pool
