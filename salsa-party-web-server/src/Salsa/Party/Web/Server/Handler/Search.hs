{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Text.Printf
import Text.Time.Pretty

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
  case queryFormCoordinates of
    Just coordinates -> searchResultPage queryFormDay queryFormAddress coordinates
    Nothing -> case queryFormAddress of
      Just address -> do
        redirect
          ( SearchR address,
            [ ("day", T.pack $ formatTime defaultTimeLocale "%F" day)
              | day <- maybeToList queryFormDay
            ]
          )
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
  withNavBar $(widgetFile "search")

-- For a given day and a given place,
-- find all parties sorted by distance.
searchQuery :: MonadIO m => Day -> Coordinates -> SqlPersistT m [(Entity Party, Entity Place)]
searchQuery day Coordinates {..} =
  E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyDay E.==. E.val day)
      let latDiff = p E.^. PlaceLat E.-. E.val coordinatesLat
      let lonDiff = p E.^. PlaceLon E.-. E.val coordinatesLon
      let latDiffSquared = latDiff E.*. latDiff
      let lonDiffSquared = lonDiff E.*. lonDiff
      -- Luckily the square function is monotone so we don't need to sqrt here
      let distSquared = latDiffSquared E.+. lonDiffSquared
      E.orderBy [E.asc distSquared]
      pure (party, p)
