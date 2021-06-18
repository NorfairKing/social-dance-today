{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import Control.Monad
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Distance
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

data QueryForm = QueryForm
  { queryFormAddress :: Maybe Text,
    queryFormCoordinates :: Maybe Coordinates,
    queryFormDay :: Maybe Day
  }
  deriving (Show, Eq, Generic)

queryForm :: FormInput Handler QueryForm
queryForm =
  QueryForm
    <$> iopt textField "address"
    <*> ( liftA2 Coordinates
            <$> (fmap realToFrac <$> iopt doubleField "latitude")
            <*> (fmap realToFrac <$> iopt doubleField "longitude")
        )
    <*> iopt dayField "day"

getQueryR :: Handler Html
getQueryR = do
  QueryForm {..} <- runInputGet queryForm
  case queryFormAddress of
    Just address -> do
      redirect
        ( SearchR address,
          [ ("day", T.pack $ formatTime defaultTimeLocale "%F" day)
            | day <- maybeToList queryFormDay
          ]
        )
    Nothing -> case queryFormCoordinates of
      Just coordinates -> searchResultPage queryFormDay queryFormAddress coordinates
      Nothing -> invalidArgs ["Must supply either an address or coordinates."]

getSearchR :: Text -> Handler Html
getSearchR query = do
  Entity _ place <- lookupPlace query
  searchResultPageWithDay (Just (placeQuery place)) (placeCoordinates place)

searchResultPageWithDay :: Maybe Text -> Coordinates -> Handler Html
searchResultPageWithDay mAddress coordinates = do
  md <- lookupGetParam "day"
  let mDay = md >>= parseTimeM True defaultTimeLocale "%F" . T.unpack
  searchResultPage mDay mAddress coordinates

searchResultPage :: Maybe Day -> Maybe Text -> Coordinates -> Handler Html
searchResultPage mDay mAddress coordinates = do
  today <- liftIO $ utctDay <$> getCurrentTime -- today
  let day = fromMaybe today mDay
  let prevDay = addDays (- 1) day
  let nextDay = addDays 1 day
  let toDouble :: Nano -> Double
      toDouble = realToFrac
  parties <- runDB $ searchQuery day coordinates
  withNavBar $ do
    setTitle $
      mconcat
        [ "Social dance parties around ",
          case mAddress of
            Just address -> toHtml address
            Nothing -> "your location",
          " ",
          toHtml $ case mDay of
            Nothing -> "today"
            Just d -> "on " <> formatTime defaultTimeLocale prettyDayFormat d
        ]
    setDescription $
      mconcat
        [ "This is our list of social dance parties in and around ",
          case mAddress of
            Just address -> address
            Nothing -> "your location",
          " ",
          T.pack $ case mDay of
            Nothing -> "today"
            Just d -> "on " <> formatTime defaultTimeLocale prettyDayFormat d,
          ".",
          "Should you wish to see your parties featured here, please make an account and submit your party for free!"
        ]
    $(widgetFile "search")

-- For a given day and a given place,
-- find all parties sorted by distance.
searchQuery :: MonadIO m => Day -> Coordinates -> SqlPersistT m [(Entity Party, Entity Place, Maybe CASKey)]
searchQuery day coordinates@Coordinates {..} = do
  rawResults <- E.select $
    E.from $ \(party `E.InnerJoin` p `E.LeftOuterJoin` mPoster) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.on (E.just (party E.^. PartyId) E.==. mPoster E.?. PosterParty)
      E.where_ (party E.^. PartyDay E.==. E.val day)
      let latDiff = p E.^. PlaceLat E.-. E.val coordinatesLat
      let lonDiff = p E.^. PlaceLon E.-. E.val coordinatesLon
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
      let latDiffSquared = latDiff E.*. latDiff
      let lonDiffSquared = lonDiff E.*. lonDiff
      -- Luckily the square function is monotone so we don't need to sqrt here
      let distSquared = latDiffSquared E.+. lonDiffSquared
      E.orderBy [E.asc distSquared]
      pure (party, p, mPoster E.?. PosterKey)

  pure $ postProcess coordinates rawResults

-- One degree longitude is 111km
roughMaxLatDistance :: Nano
roughMaxLatDistance = realToFrac maximumDistance / 111_000

-- Five degrees longitude is 555km at the equator and about 100km in north svalbard
roughMaxLonDistance :: Nano
roughMaxLonDistance = 5 * realToFrac maximumDistance / 111_000

postProcess ::
  Coordinates ->
  [(Entity Party, Entity Place, E.Value (Maybe CASKey))] ->
  [(Entity Party, Entity Place, Maybe CASKey)]
postProcess coordinates = mapMaybe $ \(party, place, E.Value mCasKey) -> do
  guard $
    coordinates `distanceTo` placeCoordinates (entityVal place)
      <= maximumDistance
  pure (party, place, mCasKey)

maximumDistance :: Double
maximumDistance = 100_000 -- 100 km
