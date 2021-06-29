{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Query where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Distance
import Salsa.Party.Web.Server.Handler.Import

data Result
  = External (Entity ExternalEvent) (Entity Place)
  | Internal (Entity Party) (Entity Place) (Maybe CASKey)
  deriving (Show, Eq)

-- For a begin day end day (inclusive) and a given place, find all parties per
-- day sorted by distance, and with external parties at the end in any case.
searchQuery :: MonadIO m => Day -> Day -> Coordinates -> SqlPersistT m (Map Day [Result])
searchQuery begin end coordinates@Coordinates {..} = do
  rawPartyResults <- E.select $
    E.from $ \((party `E.InnerJoin` p)) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ $ dayLimit (party E.^. PartyDay) begin end
      distanceEstimationQuery coordinates p
      pure (party, p)

  -- Post-process the distance before we fetch images so we don't fetch too many images.
  let partyResultsWithoutImages = postProcessParties coordinates rawPartyResults
  partyResultsWithImages <-
    forM partyResultsWithoutImages $ \(partyEntity@(Entity partyId _), placeEntity) -> do
      mKey <- getPosterForParty partyId
      pure (partyDay $ entityVal partyEntity, (partyEntity, placeEntity, mKey))

  rawExternalEventResults <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` p) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. p E.^. PlaceId)
      E.where_ $ dayLimit (externalEvent E.^. ExternalEventDay) begin end
      distanceEstimationQuery coordinates p
      pure (externalEvent, p)

  let internalResults = makeGroupedByDay partyResultsWithImages
      externalResults = makeGroupedByDay $ postProcessExternalEvents coordinates rawExternalEventResults

  pure $
    M.unionsWith
      (++)
      [ M.map (map makeInternalResult) internalResults,
        M.map (map makeExternalResult) externalResults,
        M.fromList $ [(d, []) | d <- [begin .. end]] -- Just to make sure there are no missing days.
      ]

dayLimit :: E.SqlExpr (E.Value Day) -> Day -> Day -> E.SqlExpr (E.Value Bool)
dayLimit dayExp begin end =
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
  E.where_ $
    E.between
      lat
      (E.val (coordinatesLat - roughMaxLatDistance), E.val (coordinatesLat + roughMaxLatDistance))
  E.where_ $
    E.between
      lon
      (E.val (coordinatesLon - roughMaxLonDistance), E.val (coordinatesLon + roughMaxLonDistance))

  let latDiff = lat E.-. E.val coordinatesLat
  let lonDiff = lon E.-. E.val coordinatesLon
  let latDiffSquared = latDiff E.*. latDiff
  let lonDiffSquared = lonDiff E.*. lonDiff
  -- Luckily the square function is monotone so we don't need to sqrt here
  let distSquared = latDiffSquared E.+. lonDiffSquared
  E.orderBy [E.asc distSquared]

-- One degree longitude is 111km
roughMaxLatDistance :: Nano
roughMaxLatDistance = realToFrac maximumDistance / 111_000

-- Five degrees longitude is 555km at the equator and about 100km in north svalbard
roughMaxLonDistance :: Nano
roughMaxLonDistance = 5 * realToFrac maximumDistance / 111_000

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
  [(Day, (Entity ExternalEvent, Entity Place))]
postProcessExternalEvents coordinates =
  mapMaybe $
    \(externalEvent, place) -> do
      guard $
        coordinates `distanceTo` placeCoordinates (entityVal place)
          <= maximumDistance
      pure (externalEventDay $ entityVal externalEvent, (externalEvent, place))

makeExternalResult :: (Entity ExternalEvent, Entity Place) -> Result
makeExternalResult (externalEvent, place) = External externalEvent place

maximumDistance :: Double
maximumDistance = 100_000 -- 100 km

makeGroupedByDay :: forall eTup. [(Day, eTup)] -> Map Day [eTup]
makeGroupedByDay = foldr go M.empty -- This could be falter with a fold
  where
    go :: (Day, eTup) -> Map Day [eTup] -> Map Day [eTup]
    go (d, eTup) = M.alter go' d
      where
        go' :: Maybe [eTup] -> Maybe [eTup]
        go' Nothing = Just [eTup]
        go' (Just tups) = Just $ eTup : tups
