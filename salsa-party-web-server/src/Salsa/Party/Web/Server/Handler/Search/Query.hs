{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Search.Query where

import Control.Monad
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Distance
import Salsa.Party.Web.Server.Handler.Import

data SearchResults = SearchResults
  { searchResultsParties :: [(Entity Party, Entity Place, Maybe CASKey)],
    searchResultsExternalEvents :: [(Entity ExternalEvent, Entity Place)]
  }
  deriving (Show, Eq)

-- For a given day and a given place,
-- find all parties sorted by distance.
searchQuery :: MonadIO m => Day -> Coordinates -> SqlPersistT m SearchResults
searchQuery day coordinates@Coordinates {..} = do
  rawPartyResults <- E.select $
    E.from $ \(party `E.InnerJoin` p `E.LeftOuterJoin` mPoster) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.on (E.just (party E.^. PartyId) E.==. mPoster E.?. PosterParty)
      E.where_ (party E.^. PartyDay E.==. E.val day)
      distanceEstimationQuery coordinates p
      pure (party, p, mPoster E.?. PosterKey)

  rawExternalEventResults <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` p) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. p E.^. PlaceId)
      E.where_ (externalEvent E.^. ExternalEventDay E.==. E.val day)
      distanceEstimationQuery coordinates p
      pure (externalEvent, p)

  pure
    SearchResults
      { searchResultsParties = postProcessParties coordinates rawPartyResults,
        searchResultsExternalEvents = postProcessExternalEvents coordinates rawExternalEventResults
      }

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
  [(Entity Party, Entity Place, E.Value (Maybe CASKey))] ->
  [(Entity Party, Entity Place, Maybe CASKey)]
postProcessParties coordinates = mapMaybe $ \(party, place, E.Value mCasKey) -> do
  guard $
    coordinates `distanceTo` placeCoordinates (entityVal place)
      <= maximumDistance
  pure (party, place, mCasKey)

postProcessExternalEvents ::
  Coordinates ->
  [(Entity ExternalEvent, Entity Place)] ->
  [(Entity ExternalEvent, Entity Place)]
postProcessExternalEvents coordinates = mapMaybe $ \(externalEvent, place) -> do
  guard $
    coordinates `distanceTo` placeCoordinates (entityVal place)
      <= maximumDistance
  pure (externalEvent, place)

maximumDistance :: Double
maximumDistance = 100_000 -- 100 km
