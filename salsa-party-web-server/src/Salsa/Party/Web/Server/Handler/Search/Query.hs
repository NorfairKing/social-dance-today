{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Search.Query where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Cache as Cache
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Insertion as V
import Database.Esqueleto.Experimental
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Search.Deduplication
import Salsa.Party.Web.Server.Handler.Search.Types
import Text.Show.Pretty (ppShow)

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
runSearchQueryForResults :: (MonadIO m, MonadLogger m) => SearchResultCache -> SearchQuery -> SqlPersistT m (Map Day (Vector Result))
runSearchQueryForResults searchResultCache searchQuery = do
  mCachedResults <- liftIO $ Cache.lookup searchResultCache searchQuery
  case mCachedResults of
    Just cachedResults -> do
      logDebugN $
        T.pack $
          unlines
            [ "Found cached search results, not doing search.",
              "query:",
              ppShow searchQuery
            ]
      pure cachedResults
    Nothing -> do
      logDebugN $
        T.pack $
          unlines
            [ "No cached search results, doing search.",
              "query:",
              ppShow searchQuery
            ]
      results <- runUncachedSearchQueryForResults searchQuery
      liftIO $ Cache.insert searchResultCache searchQuery results
      pure results

runUncachedSearchQueryForResults :: MonadIO m => SearchQuery -> SqlPersistT m (Map Day (Vector Result))
runUncachedSearchQueryForResults searchQuery = do
  internalResults <- runInternalSearchQuery searchQuery

  rawExternalResults <- runExternalSearchQuery searchQuery
  -- TODO deduplicate external events before fetching posters
  let externalResults = deduplicateExternalEvents internalResults rawExternalResults

  pure $
    M.map (sortResults (searchQueryCoordinates searchQuery) . V.fromList) $
      M.filter (not . null) $
        M.unionsWith
          (++)
          [ M.map (map makeInternalResult) internalResults,
            M.map (map makeExternalResult) externalResults
          ]

entityValTuple :: (Entity a, Entity b) -> (a, b)
entityValTuple (Entity _ a, Entity _ b) = (a, b)

entityValTriple :: (Entity a, Entity b, Entity c) -> (a, b, c)
entityValTriple (Entity _ a, Entity _ b, Entity _ c) = (a, b, c)

runInternalSearchQuery :: MonadIO m => SearchQuery -> SqlPersistT m (Map Day [(Organiser, Party, Place)])
runInternalSearchQuery SearchQuery {..} = do
  rawPartyResults <- fmap (map entityValTriple) $
    select $ do
      (organiser :& party :& place) <-
        from $
          table @Organiser
            `innerJoin` table @Party
            `on` ( \(organiser :& party) ->
                     organiser ^. OrganiserId ==. party ^. PartyOrganiser
                 )
            `innerJoin` table @Place
            `on` ( \(_ :& party :& place) ->
                     party ^. PartyPlace ==. place ^. PlaceId
                 )
      where_ $ dayLimit (party ^. PartyDay) searchQueryBegin searchQueryMEnd
      forM_ searchQueryDistance $ \distance -> distanceEstimationQuery distance searchQueryCoordinates place
      partySubstringQuery searchQueryDanceStyle party
      pure (organiser, party, place)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let postprocessedResults =
        maybe
          rawPartyResults
          (\dist -> postProcessParties dist searchQueryCoordinates rawPartyResults)
          searchQueryDistance

  let partyResults = map (\tup@(_, party, _) -> (partyDay party, tup)) postprocessedResults
  pure $ makeGroupedByDay partyResults

runExternalSearchQuery :: MonadIO m => SearchQuery -> SqlPersistT m (Map Day [(ExternalEvent, Place)])
runExternalSearchQuery SearchQuery {..} = do
  rawExternalEventResults <- fmap (map entityValTuple) $
    select $ do
      (externalEvent :& place) <-
        from $
          table @ExternalEvent `innerJoin` table @Place
            `on` ( \(externalEvent :& place) ->
                     externalEvent ^. ExternalEventPlace ==. place ^. PlaceId
                 )
      where_ $ dayLimit (externalEvent ^. ExternalEventDay) searchQueryBegin searchQueryMEnd
      forM_ searchQueryDistance $ \distance -> distanceEstimationQuery distance searchQueryCoordinates place
      externalEventSubstringQuery searchQueryDanceStyle externalEvent
      pure (externalEvent, place)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let postprocessedResults =
        maybe
          rawExternalEventResults
          (\dist -> postProcessExternalEvents dist searchQueryCoordinates rawExternalEventResults)
          searchQueryDistance

  let externalEventResults = flip map postprocessedResults $ \tup@(externalEvent, _) -> (externalEventDay externalEvent, tup)

  pure $
    deduplicateExternalEventsExternally $
      makeGroupedByDay externalEventResults

dayLimit :: SqlExpr (Value Day) -> Day -> Maybe Day -> SqlExpr (Value Bool)
dayLimit dayExp begin mEnd =
  case mEnd of
    Nothing -> dayExp >=. val begin
    Just end ->
      if begin == end
        then dayExp ==. val begin
        else
          between
            dayExp
            ( val begin,
              val end
            )

distanceEstimationQuery :: Word -> Coordinates -> SqlExpr (Entity Place) -> SqlQuery ()
distanceEstimationQuery maximumDistance Coordinates {..} p = do
  let lat = p ^. PlaceLat
  let lon = p ^. PlaceLon
  -- We want a very rough filter of parties by distance.
  -- What follows here is a rough estimate
  latitudeBetweenQuery maximumDistance lat coordinatesLat
  longitudeBetweenQuery maximumDistance lon coordinatesLon

  -- We used to have the following sorting function to sort by distance here.
  -- It turns out that we prefer to do this in Haskell, after deduplication, instead.
  --
  -- let latDiff = lat -. val coordinatesLat
  -- let lonDiff = lon -. val coordinatesLon
  -- let latDiffSquared = latDiff *. latDiff
  -- let lonDiffSquared = lonDiff *. lonDiff
  -- -- Luckily the square function is monotone so we don't need to sqrt here
  -- -- We need to use 'unsafeSqlBinOp " + "' because the two values are of different types.
  -- let distSquared :: SqlExpr (Value Coord)
  --     distSquared = unsafeSqlBinOp " + " latDiffSquared lonDiffSquared
  -- orderBy [asc distSquared]
  pure ()

latitudeBetweenQuery :: Word -> SqlExpr (Value Latitude) -> Latitude -> SqlQuery ()
latitudeBetweenQuery maximumDistance latExpr coordinatesLat =
  let mUpperBound = mkLatitude (unLatitude coordinatesLat + roughMaxLatDistance maximumDistance)
      mLowerBound = mkLatitude (unLatitude coordinatesLat - roughMaxLatDistance maximumDistance)
   in case (mLowerBound, mUpperBound) of
        -- Both the upper bound was too high AND the lower bound was too low, that means we want everything.
        (Nothing, Nothing) -> pure ()
        -- Both upper bound and lower bound are within range, we need only one between
        (Just lower, Just upper) -> where_ $ between latExpr (val lower, val upper)
        -- FIXME: This is wrong on the south pole.
        (Nothing, Just upper) -> where_ $ between latExpr (val minBound, val upper)
        -- FIXME: This is wrong on the north pole.
        (Just lower, Nothing) -> where_ $ between latExpr (val lower, val maxBound)

longitudeBetweenQuery :: Word -> SqlExpr (Value Longitude) -> Longitude -> SqlQuery ()
longitudeBetweenQuery maximumDistance lonExpr coordinatesLon =
  let westLimit = unLongitude coordinatesLon + roughMaxLonDistance maximumDistance
      eastLimit = unLongitude coordinatesLon - roughMaxLonDistance maximumDistance
      mUpperBound = mkLongitude westLimit
      mLowerBound = mkLongitude eastLimit
   in case (mLowerBound, mUpperBound) of
        -- Both the upper bound was too high AND the lower bound was too low, that means we want everything.
        (Nothing, Nothing) -> pure ()
        -- Both upper bound and lower bound are within range, we need only one between
        (Just lower, Just upper) -> where_ $ between lonExpr (val lower, val upper)
        -- The lower bound didn't exist.
        -- That means we're on the western boundary of the longitude wrap-around line.
        -- We'll need an 'or' condition.
        -- One that goes from the western boundary to the east,
        -- and one that goes from the west to the eastern boundary.
        (Nothing, Just upper) -> do
          -- This is the eastern half of the condition.
          let easternHalfCondition = between lonExpr (val minBound, val upper)
          let diff = abs $ unLongitude minBound - eastLimit
          let mBound = mkLongitude (unLongitude maxBound - diff)
          -- This is the western half of the condition.
          let mWesternHalfCondition = (\bound -> between lonExpr (val bound, val maxBound)) <$> mBound
          let condition = case mWesternHalfCondition of
                Nothing -> easternHalfCondition
                Just westernHalfCondition -> easternHalfCondition ||. westernHalfCondition
          where_ condition
        -- The lower bound didn't exist.
        -- That means we're on the eastern boundary of the longitude wrap-around line.
        -- We'll need an 'or' condition.
        -- One that goes from the west to the eastern boundary,
        -- and one that goes from the western boundary to the east.
        (Just lower, Nothing) -> do
          -- This is the western half of the condition.
          let westernHalfCondition = between lonExpr (val lower, val maxBound)
          let diff = abs $ unLongitude maxBound - westLimit
          let mBound = mkLongitude (unLongitude minBound + diff)
          -- This is the eastern half of the condition.
          let mEasternHalfCondition = (\bound -> between lonExpr (val minBound, val bound)) <$> mBound
          let condition = case mEasternHalfCondition of
                Nothing -> westernHalfCondition
                Just easternHalfCondition -> easternHalfCondition ||. westernHalfCondition
          where_ condition

-- One degree latitude is 111km
roughMaxLatDistance :: Word -> Coord
roughMaxLatDistance maximumDistance = fixedToCoord $ fromIntegral maximumDistance / 111_000

-- Five degrees longitude is 555km at the equator and about 100km in north svalbard
roughMaxLonDistance :: Word -> Coord
roughMaxLonDistance maximumDistance = fixedToCoord $ 5 * fromIntegral maximumDistance / 111_000

partySubstringQuery :: Maybe DanceStyle -> SqlExpr (Entity Party) -> SqlQuery ()
partySubstringQuery = substringQueryHelper PartyTitle PartyDescription

externalEventSubstringQuery :: Maybe DanceStyle -> SqlExpr (Entity ExternalEvent) -> SqlQuery ()
externalEventSubstringQuery = substringQueryHelper ExternalEventTitle ExternalEventDescription

-- Make sure this matches 'Salsa.Party.DB.DanceStyle.guessDanceStyles'
substringQueryHelper ::
  PersistEntity entity =>
  EntityField entity Text ->
  EntityField entity (Maybe Text) ->
  Maybe DanceStyle ->
  SqlExpr (Entity entity) ->
  SqlQuery ()
substringQueryHelper entityTitle entityDescription mDanceStyle entity =
  forM_ mDanceStyle $ \danceStyle ->
    where_ $
      orExpr $
        flip map (danceStyleQueryStrings danceStyle) $ \danceStyleQueryString ->
          let substringVal = val ("%" <> danceStyleQueryString <> "%")
           in (||.)
                (like (entity ^. entityTitle) substringVal)
                ( like
                    ( coalesceDefault
                        [entity ^. entityDescription]
                        (val "")
                    )
                    substringVal
                )

orExpr :: [SqlExpr (Value Bool)] -> SqlExpr (Value Bool)
orExpr = \case
  [] -> val True
  [e] -> e
  (e : es) -> e ||. orExpr es

postProcessParties ::
  Word ->
  Coordinates ->
  [(Organiser, Party, Place)] ->
  [(Organiser, Party, Place)]
postProcessParties maximumDistance coordinates =
  mapMaybe $ \(organiser, party, place) -> do
    guard $ coordinates `distanceTo` placeCoordinates place <= maximumDistance
    pure (organiser, party, place)

makeInternalResult :: (Organiser, Party, Place) -> Result
makeInternalResult (organiser, party, place) = Internal organiser party place

postProcessExternalEvents ::
  Word ->
  Coordinates ->
  [(ExternalEvent, Place)] ->
  [(ExternalEvent, Place)]
postProcessExternalEvents maximumDistance coordinates =
  mapMaybe $ \(externalEvent, place) -> do
    guard $ coordinates `distanceTo` placeCoordinates place <= maximumDistance
    pure (externalEvent, place)

makeExternalResult :: (ExternalEvent, Place) -> Result
makeExternalResult (externalEvent, place) = External externalEvent place

sortResults :: Coordinates -> Vector Result -> Vector Result
sortResults coordinates =
  V.modify $
    V.sortBy $
      mconcat
        [ comparing (Down . isInternal), -- Internal results always go first.
          comparing distanceToResult -- Sort by distance next.
        ]
  where
    isInternal = \case
      Internal _ _ _ -> True
      External _ _ -> False
    distanceToResult = \case
      External _ place -> placeCoordinates place `distanceTo` coordinates
      Internal _ _ place -> placeCoordinates place `distanceTo` coordinates

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
          searchQueryDistance = Just maximumDistance,
          searchQueryDanceStyle = Nothing
        }
