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

nullSearchResults :: Map Day [Result] -> Bool
nullSearchResults = (== 0) . countSearchResults -- Not the same as M.null!

countSearchResults :: Map Day [Result] -> Int
countSearchResults = M.foldl (+) 0 . M.map length

-- TODO this can be optimised
-- We can probably use a count query, and there's definitely no need to fetch the posters for example
noDataQuery :: MonadIO m => Coordinates -> SqlPersistT m Bool -- True means no data
noDataQuery coordinates = do
  today <- liftIO $ utctDay <$> getCurrentTime
  nullSearchResults <$> searchQuery today Nothing coordinates

data Result
  = External (Entity ExternalEvent) (Entity Place) (Maybe CASKey)
  | Internal (Entity Party) (Entity Place) (Maybe CASKey)
  deriving (Show, Eq)

-- For a begin day end day (inclusive) and a given place, find all parties per
-- day sorted by distance, and with external parties at the end in any case.
searchQuery :: MonadIO m => Day -> Maybe Day -> Coordinates -> SqlPersistT m (Map Day [Result])
searchQuery begin mEnd coordinates@Coordinates {..} = do
  rawPartyResults <- E.select $
    E.from $ \((party `E.InnerJoin` p)) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ $ dayLimit (party E.^. PartyDay) begin mEnd
      distanceEstimationQuery coordinates p
      pure (party, p)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let partyResultsWithoutImages = postProcessParties coordinates rawPartyResults
  partyResultsWithImages <-
    forM partyResultsWithoutImages $ \(partyEntity@(Entity partyId party), placeEntity) -> do
      mKey <- getPosterForParty partyId
      pure (partyDay party, (partyEntity, placeEntity, mKey))

  rawExternalEventResults <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` p) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. p E.^. PlaceId)
      E.where_ $ dayLimit (externalEvent E.^. ExternalEventDay) begin mEnd
      distanceEstimationQuery coordinates p
      pure (externalEvent, p)

  -- TODO deduplicate external events before fetching posters
  let externalEventResultsWithoutImages = postProcessExternalEvents coordinates rawExternalEventResults
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
  [(Entity Party, Entity Place)] ->
  [(Entity Party, Entity Place)]
postProcessParties coordinates =
  mapMaybe $
    \(party, place) -> do
      guard $
        coordinates `distanceTo` placeCoordinates (entityVal place)
          <= maximumDistance
      pure (party, place)

makeInternalResult :: (Entity Party, Entity Place, Maybe CASKey) -> Result
makeInternalResult (party, place, mCasKey) = Internal party place mCasKey

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
