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
  rawResults <- E.select $
    E.from $ \(party `E.InnerJoin` p `E.LeftOuterJoin` mPoster) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.on (E.just (party E.^. PartyId) E.==. mPoster E.?. PosterParty)
      E.where_ (party E.^. PartyDay E.==. E.val day)
      -- We want a very rough filter of parties by distance.
      -- What follows here is a rough estimate
      E.where_ $
        E.between
          (p E.^. PlaceLat)
          (E.val (coordinatesLat - roughMaxLatDistance), E.val (coordinatesLat + roughMaxLatDistance))
      E.where_ $
        E.between
          (p E.^. PlaceLon)
          (E.val (coordinatesLon - roughMaxLonDistance), E.val (coordinatesLon + roughMaxLonDistance))

      let latDiff = p E.^. PlaceLat E.-. E.val coordinatesLat
      let lonDiff = p E.^. PlaceLon E.-. E.val coordinatesLon
      let latDiffSquared = latDiff E.*. latDiff
      let lonDiffSquared = lonDiff E.*. lonDiff
      -- Luckily the square function is monotone so we don't need to sqrt here
      let distSquared = latDiffSquared E.+. lonDiffSquared
      E.orderBy [E.asc distSquared]
      pure (party, p, mPoster E.?. PosterKey)

  pure SearchResults {searchResultsParties = postProcessParties coordinates rawResults, searchResultsExternalEvents = []}

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

maximumDistance :: Double
maximumDistance = 100_000 -- 100 km
