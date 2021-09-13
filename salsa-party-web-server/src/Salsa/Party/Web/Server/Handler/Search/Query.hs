{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Query where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp)
import Salsa.Party.DB.Coordinates
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Deduplication

data SearchQuery = SearchQuery
  { searchQueryBegin :: !Day,
    searchQueryMEnd :: !(Maybe Day),
    searchQueryCoordinates :: !Coordinates
  }
  deriving (Show, Eq, Generic)

nullSearchResults :: Map Day [Result] -> Bool
nullSearchResults = (== 0) . countSearchResults -- Not the same as M.null!

countSearchResults :: Map Day [Result] -> Int
countSearchResults = M.foldl (+) 0 . M.map length

data Result
  = External (Entity ExternalEvent) (Entity Place) (Maybe CASKey)
  | Internal (Entity Organiser) (Entity Party) (Entity Place) (Maybe CASKey)
  deriving (Show, Eq)

-- For a begin day end day (inclusive) and a given place, find all parties per
-- day sorted by distance, and with external parties at the end in any case.
runSearchQuery :: MonadIO m => SearchQuery -> SqlPersistT m (Map Day [Result])
runSearchQuery SearchQuery {..} = do
  rawPartyResults <- E.select $
    E.from $ \((organiser `E.InnerJoin` party `E.InnerJoin` place)) -> do
      E.on (organiser E.^. OrganiserId E.==. party E.^. PartyOrganiser)
      E.on (party E.^. PartyPlace E.==. place E.^. PlaceId)
      E.where_ $ dayLimit (party E.^. PartyDay) searchQueryBegin searchQueryMEnd
      distanceEstimationQuery searchQueryCoordinates place
      pure (organiser, party, place)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let partyResultsWithoutImages = postProcessParties searchQueryCoordinates rawPartyResults
  partyResultsWithImages <-
    forM partyResultsWithoutImages $ \(organiserEntity, partyEntity@(Entity partyId party), placeEntity) -> do
      mKey <- getPosterForParty partyId
      pure (partyDay party, (organiserEntity, partyEntity, placeEntity, mKey))

  rawExternalEventResults <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` place) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
      E.where_ $ dayLimit (externalEvent E.^. ExternalEventDay) searchQueryBegin searchQueryMEnd
      distanceEstimationQuery searchQueryCoordinates place
      pure (externalEvent, place)

  -- TODO deduplicate external events before fetching posters
  let externalEventResultsWithoutImages = postProcessExternalEvents searchQueryCoordinates rawExternalEventResults
  externalEventResultsWithImages <-
    forM externalEventResultsWithoutImages $ \(externalEventEntity@(Entity externalEventId externalEvent), placeEntity) -> do
      mKey <- getPosterForExternalEvent externalEventId
      pure (externalEventDay externalEvent, (externalEventEntity, placeEntity, mKey))

  let internalResults = makeGroupedByDay partyResultsWithImages
      externalResults =
        deduplicateExternalEvents internalResults $
          deduplicateExternalEventsExternally $
            makeGroupedByDay externalEventResultsWithImages

  pure $
    M.filter (not . null) $
      M.unionsWith
        (++)
        [ M.map (map makeInternalResult) internalResults,
          M.map (map makeExternalResult) externalResults
        ]

dayLimit :: E.SqlExpr (E.Value Day) -> Day -> Maybe Day -> E.SqlExpr (E.Value Bool)
dayLimit dayExp begin mEnd =
  case mEnd of
    Nothing -> E.val begin E.<=. dayExp
    Just end ->
      if begin == end
        then dayExp E.==. E.val begin
        else
          E.between
            dayExp
            ( E.val begin,
              E.val end
            )

distanceEstimationQuery :: Coordinates -> E.SqlExpr (Entity Place) -> E.SqlQuery ()
distanceEstimationQuery Coordinates {..} p = do
  let lat = p E.^. PlaceLat
  let lon = p E.^. PlaceLon
  -- We want a very rough filter of parties by distance.
  -- What follows here is a rough estimate
  latitudeBetweenQuery lat coordinatesLat
  longitudeBetweenQuery lon coordinatesLon

  let latDiff = lat E.-. E.val coordinatesLat
  let lonDiff = lon E.-. E.val coordinatesLon
  let latDiffSquared = latDiff E.*. latDiff
  let lonDiffSquared = lonDiff E.*. lonDiff
  -- Luckily the square function is monotone so we don't need to sqrt here
  -- We need to use 'unsafeSqlBinOp " + "' because the two values are of different types.
  let distSquared :: E.SqlExpr (E.Value Coord)
      distSquared = unsafeSqlBinOp " + " latDiffSquared lonDiffSquared
  E.orderBy [E.asc distSquared]

latitudeBetweenQuery :: E.SqlExpr (E.Value Latitude) -> Latitude -> E.SqlQuery ()
latitudeBetweenQuery latExpr coordinatesLat =
  let mUpperBound = mkLatitude (unLatitude coordinatesLat + roughMaxLatDistance)
      mLowerBound = mkLatitude (unLatitude coordinatesLat - roughMaxLatDistance)
   in case (mLowerBound, mUpperBound) of
        -- Both the upper bound was too high AND the lower bound was too low, that means we want everything.
        (Nothing, Nothing) -> pure ()
        -- Both upper bound and lower bound are within range, we need only one between
        (Just lower, Just upper) -> E.where_ $ E.between latExpr (E.val lower, E.val upper)
        -- FIXME: This is wrong on the south pole.
        (Nothing, Just upper) -> E.where_ $ E.between latExpr (E.val minBound, E.val upper)
        -- FIXME: This is wrong on the north pole.
        (Just lower, Nothing) -> E.where_ $ E.between latExpr (E.val lower, E.val maxBound)

longitudeBetweenQuery :: E.SqlExpr (E.Value Longitude) -> Longitude -> E.SqlQuery ()
longitudeBetweenQuery lonExpr coordinatesLon =
  let mUpperBound = mkLongitude (unLongitude coordinatesLon + roughMaxLonDistance)
      mLowerBound = mkLongitude (unLongitude coordinatesLon - roughMaxLonDistance)
   in case (mLowerBound, mUpperBound) of
        -- Both the upper bound was too high AND the lower bound was too low, that means we want everything.
        (Nothing, Nothing) -> pure ()
        -- Both upper bound and lower bound are within range, we need only one between
        (Just lower, Just upper) -> E.where_ $ E.between lonExpr (E.val lower, E.val upper)
        -- FIXME: This will be wrong in west Canada
        (Nothing, Just upper) -> E.where_ $ E.between lonExpr (E.val minBound, E.val upper)
        -- FIXME: This will be wrong in east Russia
        (Just lower, Nothing) -> E.where_ $ E.between lonExpr (E.val lower, E.val maxBound)

-- One degree longitude is 111km
roughMaxLatDistance :: Coord
roughMaxLatDistance = fixedToCoord $ realToFrac maximumDistance / 111_000

-- Five degrees longitude is 555km at the equator and about 100km in north svalbard
roughMaxLonDistance :: Coord
roughMaxLonDistance = fixedToCoord $ 5 * realToFrac maximumDistance / 111_000

postProcessParties ::
  Coordinates ->
  [(Entity Organiser, Entity Party, Entity Place)] ->
  [(Entity Organiser, Entity Party, Entity Place)]
postProcessParties coordinates =
  mapMaybe $
    \(organiser, party, place) -> do
      guard $
        coordinates `distanceTo` placeCoordinates (entityVal place)
          <= maximumDistance
      pure (organiser, party, place)

makeInternalResult :: (Entity Organiser, Entity Party, Entity Place, Maybe CASKey) -> Result
makeInternalResult (organiser, party, place, mCasKey) = Internal organiser party place mCasKey

postProcessExternalEvents ::
  Coordinates ->
  [(Entity ExternalEvent, Entity Place)] ->
  [(Entity ExternalEvent, Entity Place)]
postProcessExternalEvents coordinates =
  mapMaybe $
    \(externalEvent, place) -> do
      guard $
        coordinates `distanceTo` placeCoordinates (entityVal place)
          <= maximumDistance
      pure (externalEvent, place)

makeExternalResult :: (Entity ExternalEvent, Entity Place, Maybe CASKey) -> Result
makeExternalResult (externalEvent, place, mCasKey) = External externalEvent place mCasKey

maximumDistance :: Double
maximumDistance = 50_000 -- 50 km

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
noDataQuery :: MonadIO m => Coordinates -> SqlPersistT m Bool -- True means no data
noDataQuery coordinates = do
  today <- liftIO $ utctDay <$> getCurrentTime
  nullSearchResults
    <$> runSearchQuery
      SearchQuery
        { searchQueryBegin = today,
          searchQueryMEnd = Nothing,
          searchQueryCoordinates = coordinates
        }
