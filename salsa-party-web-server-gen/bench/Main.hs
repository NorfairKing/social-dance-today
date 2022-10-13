{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Logger
import Criterion.Main as Criterion
import Data.Cache (newCache)
import Data.GenValidity
import qualified Data.Set as S
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.DB.Gen
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.Search.Types
import Salsa.Party.Web.Server.TestUtils
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.Syd

main :: IO ()
main =
  unSetupFunc salsaConnectionPoolSetupFunc $ \pool ->
    Criterion.defaultMain
      [ env
          (setupSearchData pool)
          (doSearchAtBenchmark pool)
      ]

runGen :: Gen a -> a
runGen (MkGen func) = func qcGen 30
  where
    qcGen = mkQCGen 42

setupSearchData :: ConnectionPool -> IO (Vector SearchQuery)
setupSearchData = runSqlPool setupSearchDataQuery

setupSearchDataQuery :: SqlPersistT IO (Vector SearchQuery)
setupSearchDataQuery = do
  let places = runGen $
        genUniques placeQuery 10000 $ do
          location <- elements locations
          genPlaceAroundLocation (locationPlace location)
  placeIds <- insertMany places

  let (partyPrototypes, externalEventPrototypes) = runGen $ genPrototypes placeIds

  -- Set up parties
  forM_ partyPrototypes $ \partyPrototype -> do
    uuid <- nextRandomUUID
    insert $ partyPrototype {partyUuid = uuid}

  -- Set up external events
  forM_ externalEventPrototypes $ \externalEventPrototype -> do
    uuid <- nextRandomUUID
    insert $ externalEventPrototype {externalEventUuid = uuid}

  pure $ runGen $ V.replicateM 100 genQuery

genQuery :: Gen SearchQuery
genQuery = do
  -- Make a query
  day <- genDay
  let chooseAround :: (Integral i, Random i) => i -> Gen i
      chooseAround i =
        let twentyPercent = fromIntegral i * 100 / 20 :: Double
         in choose
              ( round (fromIntegral i - twentyPercent),
                round (fromIntegral i + twentyPercent)
              )
  daysAhead <- chooseAround defaultDaysAhead
  coordinates <- do
    location <- elements locations
    genCoordinatesAround (placeCoordinates (locationPlace location))
  distance <- chooseAround defaultMaximumDistance
  pure
    SearchQuery
      { searchQueryBegin = day,
        searchQueryMEnd = Just $ addDays daysAhead day,
        searchQueryCoordinates = coordinates,
        searchQueryDistance = Just distance,
        searchQueryDanceStyle =
          -- Otherwise we have to fill the parties with substrings
          Nothing
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
  genUniques (\ExternalEvent {..} -> (externalEventImporter, externalEventKey)) 50000 $ do
    placeId <- elements placeIds
    day <- genDay
    externalEventPrototype <- genValid
    pure $
      externalEventPrototype
        { externalEventDay = day,
          externalEventPlace = placeId
        }

genUniques :: Ord b => (a -> b) -> Word -> Gen a -> Gen [a]
genUniques func n gen = go S.empty n
  where
    go acc i
      | i <= 0 = pure []
      | otherwise = do
        a <- gen
        let b = func a
        if b `S.member` acc
          then go acc i
          else (a :) <$> go (S.insert b acc) (pred i)

-- We generate days within a month, there's no point in looking accross centuries.
genDay :: Gen Day
genDay = fromGregorian 2021 06 <$> choose (1, 31)

doSearchAtBenchmark :: ConnectionPool -> Vector SearchQuery -> Benchmark
doSearchAtBenchmark pool queries =
  bench "search" $
    nfIO $
      doSearchesAt pool queries

doSearchesAt :: ConnectionPool -> Vector SearchQuery -> IO (Vector SearchResult)
doSearchesAt pool queries =
  mapM (doSearchAt pool)

doSearchAt :: ConnectionPool -> SearchQuery -> IO SearchResult
doSearchAt pool query = do
  -- Make a new cache every time,
  -- so we benchmark the uncached performance.
  cache <- newCache Nothing
  runNoLoggingT $ runSqlPool (runSearchQuery cache query) pool
