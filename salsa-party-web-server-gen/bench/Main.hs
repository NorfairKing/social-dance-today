{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Exception
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
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.Search.Types
import Salsa.Party.Web.Server.TestUtils
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
  liftIO $ putStrLn "Generating Places"
  let places = runGen $
        genUniques placeQuery 10000 $ do
          location <- elements locations
          genPlaceAroundLocation (locationPlace location)
  placeIds <- insertMany places

  liftIO $ putStrLn "Generating importers"
  let importers = runGen $ genUniques importerMetadataName 20 genValid
  importerIds <- insertMany importers

  liftIO $ putStrLn "Generating parties and external events"
  let (partyPrototypes, externalEventPrototypes) = runGen $ genPrototypes importerIds placeIds

  -- Set up parties
  forM_ partyPrototypes $ \partyPrototype -> do
    uuid <- nextRandomUUID
    insert $ partyPrototype {partyUuid = uuid}

  -- Set up external events
  forM_ externalEventPrototypes $ \externalEventPrototype -> do
    uuid <- nextRandomUUID
    insert $ externalEventPrototype {externalEventUuid = uuid}

  liftIO $ putStrLn "Generating search queries"
  let queries = runGen $ V.replicateM 100 genQuery
  liftIO $ evaluate $ force queries

genQuery :: Gen SearchQuery
genQuery = do
  -- Make a query
  day <- genDay
  daysAhead <- choose (defaultDaysAhead - 2, defaultDaysAhead + 2)
  coordinates <- do
    location <- elements locations
    genCoordinatesAround (placeCoordinates (locationPlace location))
  distance <- choose (defaultMaximumDistance - 20_000, defaultMaximumDistance + 20_000)
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

genPrototypes :: [ImporterMetadataId] -> [PlaceId] -> Gen ([Party], [ExternalEvent])
genPrototypes importers placeIds =
  (,)
    <$> genPartyPrototypes placeIds
    <*> genExternalEventPrototypes importers placeIds

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

genExternalEventPrototypes :: [ImporterMetadataId] -> [PlaceId] -> Gen [ExternalEvent]
genExternalEventPrototypes importers placeIds = do
  genUniques externalEventKey 50000 $ do
    placeId <- elements placeIds
    importer <- elements importers
    day <- genDay
    externalEventPrototype <- genValid
    pure $
      externalEventPrototype
        { externalEventDay = day,
          externalEventPlace = placeId,
          externalEventImporter = importer
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
          then scale succ $ go acc i
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
doSearchesAt pool = mapM (doSearchAt pool)

doSearchAt :: ConnectionPool -> SearchQuery -> IO SearchResult
doSearchAt pool query = do
  -- Make a new cache every time,
  -- so we benchmark the uncached performance.
  cache <- newCache Nothing
  runNoLoggingT $ runSqlPool (runSearchQuery cache query) pool
