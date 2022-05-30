{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Query where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Cache as Cache
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Text as T
import Data.Time
import qualified Database.Esqueleto.Legacy as E
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Deduplication
import Salsa.Party.Web.Server.Handler.Search.Types
import System.Clock (TimeSpec)
import qualified System.Clock as TimeSpec

runSearchQuery :: (MonadIO m, MonadLogger m) => SearchResultCache -> SearchQuery -> SqlPersistT m SearchResult
runSearchQuery searchResultCache searchQuery@SearchQuery {..} = do
  results <- runSearchQueryForResults searchResultCache searchQuery
  if nullSearchResults results
    then case searchQueryDistance of
      Nothing -> pure $ ResultsFound results
      Just maximumDistance -> do
        noDataYet <- noDataQuery searchResultCache searchQueryCoordinates maximumDistance
        pure $
          if noDataYet
            then NoDataYet
            else ResultsFound results
    else pure $ ResultsFound results

-- For a begin day end day (inclusive) and a given place, find all parties per
-- day sorted by distance, and with external parties at the end in any case.
runSearchQueryForResults :: (MonadIO m, MonadLogger m) => SearchResultCache -> SearchQuery -> SqlPersistT m (Map Day [Result])
runSearchQueryForResults searchResultCache searchQuery = do
  mResults <- liftIO $ Cache.lookup searchResultCache searchQuery
  case mResults of
    Just results -> do
      logDebugN $
        T.pack $
          unlines
            [ "Found cached result, not doing search.",
              "query:",
              ppShow searchQuery
            ]
      pure results
    Nothing -> do
      logDebugN $
        T.pack $
          unlines
            [ "No cached result, doing search.",
              "query:",
              ppShow searchQuery
            ]
      results <- runUncachedSearchQueryForResults searchQuery
      liftIO $ Cache.insert' searchResultCache (Just searchResultCacheTimeSpec) searchQuery results
      pure results

searchResultCacheTimeSpec :: TimeSpec
searchResultCacheTimeSpec = TimeSpec.fromNanoSecs $ 60 * 60 * 1_000_000_000

runUncachedSearchQueryForResults :: MonadIO m => SearchQuery -> SqlPersistT m (Map Day [Result])
runUncachedSearchQueryForResults SearchQuery {..} = do
  rawPartyResults <- E.select $
    E.from $ \((organiser `E.InnerJoin` party `E.InnerJoin` place)) -> do
      E.on (organiser E.^. OrganiserId E.==. party E.^. PartyOrganiser)
      E.on (party E.^. PartyPlace E.==. place E.^. PlaceId)
      E.where_ $ dayLimit (party E.^. PartyDay) searchQueryBegin searchQueryMEnd
      forM_ searchQueryDistance $ \distance -> distanceEstimationQuery distance searchQueryCoordinates place
      pure (organiser, party, place)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let partyResultsWithoutImages = maybe rawPartyResults (\dist -> postProcessParties dist searchQueryCoordinates rawPartyResults) searchQueryDistance
  partyResultsWithImages <-
    forM partyResultsWithoutImages $ \(organiserEntity, partyEntity@(Entity partyId party), placeEntity) -> do
      mKey <- getPosterForParty partyId
      pure (partyDay party, (organiserEntity, partyEntity, placeEntity, mKey))

  rawExternalEventResults <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` place) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
      E.where_ $ dayLimit (externalEvent E.^. ExternalEventDay) searchQueryBegin searchQueryMEnd
      forM_ searchQueryDistance $ \distance -> distanceEstimationQuery distance searchQueryCoordinates place
      pure (externalEvent, place)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let externalEventResultsWithoutImages = maybe rawExternalEventResults (\dist -> postProcessExternalEvents dist searchQueryCoordinates rawExternalEventResults) searchQueryDistance

  externalEventResultsWithImages <-
    forM externalEventResultsWithoutImages $ \(externalEventEntity@(Entity externalEventId externalEvent), placeEntity) -> do
      mKey <- getPosterForExternalEvent externalEventId
      pure (externalEventDay externalEvent, (externalEventEntity, placeEntity, mKey))

  let internalResults = makeGroupedByDay partyResultsWithImages
      -- TODO deduplicate external events before fetching posters
      externalResults =
        deduplicateExternalEvents internalResults $
          deduplicateExternalEventsExternally $
            makeGroupedByDay externalEventResultsWithImages

  pure $
    M.map (sortResults searchQueryCoordinates) $
      M.filter (not . null) $
        M.unionsWith
          (++)
          [ M.map (map makeInternalResult) internalResults,
            M.map (map makeExternalResult) externalResults
          ]

dayLimit :: E.SqlExpr (E.Value Day) -> Day -> Maybe Day -> E.SqlExpr (E.Value Bool)
dayLimit dayExp begin mEnd =
  case mEnd of
    Nothing -> dayExp E.>=. E.val begin
    Just end ->
      if begin == end
        then dayExp E.==. E.val begin
        else
          E.between
            dayExp
            ( E.val begin,
              E.val end
            )

distanceEstimationQuery :: Word -> Coordinates -> E.SqlExpr (Entity Place) -> E.SqlQuery ()
distanceEstimationQuery maximumDistance Coordinates {..} p = do
  let lat = p E.^. PlaceLat
  let lon = p E.^. PlaceLon
  -- We want a very rough filter of parties by distance.
  -- What follows here is a rough estimate
  latitudeBetweenQuery maximumDistance lat coordinatesLat
  longitudeBetweenQuery maximumDistance lon coordinatesLon

  -- We used to have the following sorting function to sort by distance here.
  -- It turns out that we prefer to do this in Haskell, after deduplication, instead.
  --
  -- let latDiff = lat E.-. E.val coordinatesLat
  -- let lonDiff = lon E.-. E.val coordinatesLon
  -- let latDiffSquared = latDiff E.*. latDiff
  -- let lonDiffSquared = lonDiff E.*. lonDiff
  -- -- Luckily the square function is monotone so we don't need to sqrt here
  -- -- We need to use 'unsafeSqlBinOp " + "' because the two values are of different types.
  -- let distSquared :: E.SqlExpr (E.Value Coord)
  --     distSquared = unsafeSqlBinOp " + " latDiffSquared lonDiffSquared
  -- E.orderBy [E.asc distSquared]
  pure ()

latitudeBetweenQuery :: Word -> E.SqlExpr (E.Value Latitude) -> Latitude -> E.SqlQuery ()
latitudeBetweenQuery maximumDistance latExpr coordinatesLat =
  let mUpperBound = mkLatitude (unLatitude coordinatesLat + roughMaxLatDistance maximumDistance)
      mLowerBound = mkLatitude (unLatitude coordinatesLat - roughMaxLatDistance maximumDistance)
   in case (mLowerBound, mUpperBound) of
        -- Both the upper bound was too high AND the lower bound was too low, that means we want everything.
        (Nothing, Nothing) -> pure ()
        -- Both upper bound and lower bound are within range, we need only one between
        (Just lower, Just upper) -> E.where_ $ E.between latExpr (E.val lower, E.val upper)
        -- FIXME: This is wrong on the south pole.
        (Nothing, Just upper) -> E.where_ $ E.between latExpr (E.val minBound, E.val upper)
        -- FIXME: This is wrong on the north pole.
        (Just lower, Nothing) -> E.where_ $ E.between latExpr (E.val lower, E.val maxBound)

longitudeBetweenQuery :: Word -> E.SqlExpr (E.Value Longitude) -> Longitude -> E.SqlQuery ()
longitudeBetweenQuery maximumDistance lonExpr coordinatesLon =
  let westLimit = unLongitude coordinatesLon + roughMaxLonDistance maximumDistance
      eastLimit = unLongitude coordinatesLon - roughMaxLonDistance maximumDistance
      mUpperBound = mkLongitude westLimit
      mLowerBound = mkLongitude eastLimit
   in case (mLowerBound, mUpperBound) of
        -- Both the upper bound was too high AND the lower bound was too low, that means we want everything.
        (Nothing, Nothing) -> pure ()
        -- Both upper bound and lower bound are within range, we need only one between
        (Just lower, Just upper) -> E.where_ $ E.between lonExpr (E.val lower, E.val upper)
        -- The lower bound didn't exist.
        -- That means we're on the western boundary of the longitude wrap-around line.
        -- We'll need an 'or' condition.
        -- One that goes from the western boundary to the east,
        -- and one that goes from the west to the eastern boundary.
        (Nothing, Just upper) -> do
          -- This is the eastern half of the condition.
          let easternHalfCondition = E.between lonExpr (E.val minBound, E.val upper)
          let diff = abs $ unLongitude minBound - eastLimit
          let mBound = mkLongitude (unLongitude maxBound - diff)
          -- This is the western half of the condition.
          let mWesternHalfCondition = (\bound -> E.between lonExpr (E.val bound, E.val maxBound)) <$> mBound
          let condition = case mWesternHalfCondition of
                Nothing -> easternHalfCondition
                Just westernHalfCondition -> easternHalfCondition E.||. westernHalfCondition
          E.where_ condition
        -- The lower bound didn't exist.
        -- That means we're on the eastern boundary of the longitude wrap-around line.
        -- We'll need an 'or' condition.
        -- One that goes from the west to the eastern boundary,
        -- and one that goes from the western boundary to the east.
        (Just lower, Nothing) -> do
          -- This is the western half of the condition.
          let westernHalfCondition = E.between lonExpr (E.val lower, E.val maxBound)
          let diff = abs $ unLongitude maxBound - westLimit
          let mBound = mkLongitude (unLongitude minBound + diff)
          -- This is the eastern half of the condition.
          let mEasternHalfCondition = (\bound -> E.between lonExpr (E.val minBound, E.val bound)) <$> mBound
          let condition = case mEasternHalfCondition of
                Nothing -> westernHalfCondition
                Just easternHalfCondition -> easternHalfCondition E.||. westernHalfCondition
          E.where_ condition

-- One degree latitude is 111km
roughMaxLatDistance :: Word -> Coord
roughMaxLatDistance maximumDistance = fixedToCoord $ fromIntegral maximumDistance / 111_000

-- Five degrees longitude is 555km at the equator and about 100km in north svalbard
roughMaxLonDistance :: Word -> Coord
roughMaxLonDistance maximumDistance = fixedToCoord $ 5 * fromIntegral maximumDistance / 111_000

postProcessParties ::
  Word ->
  Coordinates ->
  [(Entity Organiser, Entity Party, Entity Place)] ->
  [(Entity Organiser, Entity Party, Entity Place)]
postProcessParties maximumDistance coordinates =
  mapMaybe $ \(organiser, party, place) -> do
    guard $ coordinates `distanceTo` placeCoordinates (entityVal place) <= maximumDistance
    pure (organiser, party, place)

makeInternalResult :: (Entity Organiser, Entity Party, Entity Place, Maybe CASKey) -> Result
makeInternalResult (organiser, party, place, mCasKey) = Internal organiser party place mCasKey

postProcessExternalEvents ::
  Word ->
  Coordinates ->
  [(Entity ExternalEvent, Entity Place)] ->
  [(Entity ExternalEvent, Entity Place)]
postProcessExternalEvents maximumDistance coordinates =
  mapMaybe $ \(externalEvent, place) -> do
    guard $ coordinates `distanceTo` placeCoordinates (entityVal place) <= maximumDistance
    pure (externalEvent, place)

makeExternalResult :: (Entity ExternalEvent, Entity Place, Maybe CASKey) -> Result
makeExternalResult (externalEvent, place, mCasKey) = External externalEvent place mCasKey

sortResults :: Coordinates -> [Result] -> [Result]
sortResults coordinates =
  sortBy $
    mconcat
      [ comparing (Down . isInternal), -- Internal results always go first.
        comparing distanceToResult -- Sort by distance next.
      ]
  where
    isInternal = \case
      Internal _ _ _ _ -> True
      External _ _ _ -> False
    distanceToResult = \case
      External _ (Entity _ place) _ -> placeCoordinates place `distanceTo` coordinates
      Internal _ _ (Entity _ place) _ -> placeCoordinates place `distanceTo` coordinates

defaultMaximumDistance :: Word
defaultMaximumDistance = 50_000 -- 50 km

maximumDistanceStep :: Word
maximumDistanceStep = 1_000

minimumMaximumDistance :: Word
minimumMaximumDistance = maximumDistanceStep -- 1 km

maximumMaximumDistance :: Word
maximumMaximumDistance = 200_000 -- 200 km

makeGroupedByDay :: forall eTup. [(Day, eTup)] -> Map Day [eTup]
makeGroupedByDay = foldr go M.empty -- This could be falter with a fold
  where
    go :: (Day, eTup) -> Map Day [eTup] -> Map Day [eTup]
    go (d, eTup) = M.alter go' d
      where
        go' :: Maybe [eTup] -> Maybe [eTup]
        go' Nothing = Just [eTup]
        go' (Just tups) = Just $ eTup : tups

-- TODO this can be optimised
-- We can probably use a count query, and there's definitely no need to fetch the posters for example
noDataQuery :: (MonadIO m, MonadLogger m) => SearchResultCache -> Coordinates -> Word -> SqlPersistT m Bool -- True means no data
noDataQuery searchResultCache coordinates maximumDistance = do
  today <- liftIO $ utctDay <$> getCurrentTime
  nullSearchResults
    <$> runSearchQueryForResults
      searchResultCache
      SearchQuery
        { searchQueryBegin = today,
          searchQueryMEnd = Nothing,
          searchQueryCoordinates = coordinates,
          searchQueryDistance = Just maximumDistance
        }
