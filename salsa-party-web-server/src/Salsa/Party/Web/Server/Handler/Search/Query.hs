{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Query where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Distance
import Salsa.Party.Web.Server.Handler.Import

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
  = External (Entity ExternalEvent) (Entity Place)
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
    forM partyResultsWithoutImages $ \(partyEntity@(Entity partyId _), placeEntity) -> do
      mKey <- getPosterForParty partyId
      pure (partyDay $ entityVal partyEntity, (partyEntity, placeEntity, mKey))

  rawExternalEventResults <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` p) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. p E.^. PlaceId)
      E.where_ $ dayLimit (externalEvent E.^. ExternalEventDay) begin mEnd
      distanceEstimationQuery coordinates p
      pure (externalEvent, p)

  let internalResults = makeGroupedByDay partyResultsWithImages
      externalResults = deduplicateExternalEvents internalResults $ makeGroupedByDay $ postProcessExternalEvents coordinates rawExternalEventResults

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

-- | Find external events that look like internal events, and delete them from the external events list.
--
-- We don't like false-positives, because then we see duplicate events: External events that are the same as some internal event we have.
-- We don't like false-negatives, because then we don't see certain external events.
--
-- In general false-negatives are safer than false-positives, for the user experience.
deduplicateExternalEvents ::
  Map Day [(Entity Party, Entity Place, Maybe CASKey)] ->
  Map Day [(Entity ExternalEvent, Entity Place)] ->
  Map Day [(Entity ExternalEvent, Entity Place)]
deduplicateExternalEvents internals externals = M.differenceWith go externals internals
  where
    go ::
      [(Entity ExternalEvent, Entity Place)] ->
      [(Entity Party, Entity Place, Maybe CASKey)] ->
      Maybe [(Entity ExternalEvent, Entity Place)]
    go externalsOnDay internalsOnDay =
      -- TODO: This is a quadratic-time comparison.
      -- We rely on the assumption that there are not a lot of events happening in the same area on the same day.
      Just $
        filter
          (\externalEvent -> not $ any (isSimilarEnoughTo externalEvent) internalsOnDay)
          externalsOnDay
    isSimilarEnoughTo :: (Entity ExternalEvent, Entity Place) -> (Entity Party, Entity Place, Maybe CASKey) -> Bool
    isSimilarEnoughTo (Entity _ ExternalEvent {..}, Entity place1Id place1) (Entity _ Party {..}, Entity place2Id place2, _) =
      -- For the following conditions, keep in mind that it's already established that the two things happen on the same day.
      or
        [ -- At exactly the same location is probably the same event.
          place1Id == place2Id,
          -- If they're happening at the same address (modulo whitespace), it's also probably the same event.
          T.strip (placeQuery place1) == T.strip (placeQuery place2),
          -- If the title of two events are the same (modulo whitespace), it's also probably the same event.
          -- We rely on the assumption that different events will want to differentiate themselves from eachother
          T.strip externalEventTitle == T.strip partyTitle
        ]
