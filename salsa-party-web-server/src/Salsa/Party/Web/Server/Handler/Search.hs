{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import Control.Arrow (left)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Query

data QueryForm = QueryForm
  { queryFormAddress :: Maybe Text,
    queryFormCoordinates :: Maybe Coordinates,
    queryFormDay :: Maybe Day
  }
  deriving (Show, Eq, Generic)

addressParameter :: Text
addressParameter = "address"

latitudeParameter :: Text
latitudeParameter = "latitude"

longitudeParameter :: Text
longitudeParameter = "longitude"

dayParameter :: Text
dayParameter = "day"

queryForm :: FormInput Handler QueryForm
queryForm =
  QueryForm
    <$> iopt textField addressParameter
    <*> ( liftA2 Coordinates
            <$> iopt latitudeField "latitude"
            <*> iopt longitudeField "longitude"
        )
    <*> iopt dayField dayParameter

latitudeField :: Field Handler Latitude
latitudeField =
  checkMMap
    (\d -> pure . left (const (MsgInvalidLatitude (show d))) . mkLatitudeOrError . fixedToCoord . realToFrac $ d)
    (realToFrac . unLatitude)
    doubleField

longitudeField :: Field Handler Longitude
longitudeField =
  checkMMap
    (\d -> pure . left (const (MsgInvalidLongitude (show d))) . mkLongitudeOrError . fixedToCoord . realToFrac $ d)
    (realToFrac . unLongitude)
    doubleField

queryFormParameters :: QueryForm -> [(Text, Text)]
queryFormParameters QueryForm {..} =
  concat
    [ [(addressParameter, address) | address <- maybeToList queryFormAddress],
      concat
        [ [ (latitudeParameter, T.pack $ show coordinatesLat),
            (longitudeParameter, T.pack $ show coordinatesLon)
          ]
          | Coordinates {..} <- maybeToList queryFormCoordinates
        ],
      [(dayParameter, T.pack $ formatTime defaultTimeLocale "%F" day) | day <- maybeToList queryFormDay]
    ]

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
  searchResultsPage
    SearchParameters
      { searchParameterLocation = SearchAddress query,
        searchParameterBegin = BeginToday
      }

getSearchDayR :: Text -> Day -> Handler Html
getSearchDayR query day =
  searchResultsPage
    SearchParameters
      { searchParameterLocation = SearchAddress query,
        searchParameterBegin = BeginOn day
      }

-- | Search parameters contain everything necessary to produce the html page of search results.
--
-- We do as much computation ahead of time, but not too much.
-- For example, the 'SearchBegin' parameter could be computed ahead of time, but then we would not be able to compute a nice title and description
data SearchParameters = SearchParameters
  { searchParameterLocation :: !SearchLocation,
    searchParameterBegin :: !SearchBegin
  }
  deriving (Show, Eq, Generic)

searchParametersForm :: FormInput Handler SearchParameters
searchParametersForm =
  SearchParameters
    <$> searchParameterLocationForm
    <*> searchParameterBeginForm

data SearchLocation
  = SearchAddress !Text
  | SearchCoordinates !Coordinates
  deriving (Show, Eq, Generic)

resolveSearchLocation :: SearchLocation -> Handler Coordinates
resolveSearchLocation = \case
  SearchCoordinates coordinates -> pure coordinates
  SearchAddress address -> do
    Entity _ place <- lookupPlace address
    pure $ placeCoordinates place

data SearchBegin
  = BeginToday
  | BeginOn !Day
  deriving (Show, Eq, Generic)

searchResultsPage :: SearchParameters -> Handler Html
searchResultsPage searchParameters@SearchParameters {..} = do
  today <- liftIO $ utctDay <$> getCurrentTime -- today
  let daysAhead = 7 -- TODO turn into a parameter

  -- Resolve begin day
  let begin = case searchParameterBegin of
        BeginToday -> today
        BeginOn day -> day

  -- Resolve end day
  -- TODO incorporate search end parameter
  let end = addDays (daysAhead - 1) begin

  -- Resolve coordinates
  coordinates <- resolveSearchLocation searchParameterLocation

  -- Do the actual search
  searchResults <-
    runDB $
      runSearchQuery
        SearchQuery
          { searchQueryBegin = begin,
            searchQueryMEnd = Just end,
            searchQueryCoordinates = coordinates
          }

  -- If no results were returned, check if there was any data at all
  noData <-
    if nullSearchResults searchResults
      then runDB $ noDataQuery coordinates
      else pure False

  withNavBar $ do
    setTitleI $ searchParametersTitle searchParameters
    setDescriptionI $ searchParametersDescription searchParameters
    timeLocale <- getTimeLocale
    prettyDayFormat <- getPrettyDayFormat
    if noData
      then $(widgetFile "search-no-results")
      else do
        let prevDay = addDays (negate daysAhead) begin
        let nextDay = addDays daysAhead begin
        let days = [begin .. end]
        let pagination = $(widgetFile "search-pagination")
        $(widgetFile "search")

searchParametersTitle :: SearchParameters -> AppMessage
searchParametersTitle SearchParameters {..} =
  case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterBegin of
      BeginToday -> MsgSearchTitleAroundYourLocationToday
      BeginOn d -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat d
    SearchAddress address -> case searchParameterBegin of
      BeginToday -> MsgSearchTitleAroundAddressToday address
      BeginOn d -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat d

searchParametersDescription :: SearchParameters -> AppMessage
searchParametersDescription SearchParameters {..} =
  case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterBegin of
      BeginToday -> MsgSearchDescriptionAroundYourLocationToday
      BeginOn day -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress -> case searchParameterBegin of
      BeginToday -> MsgSearchDescriptionAroundAddressToday address
      BeginOn day -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat d
