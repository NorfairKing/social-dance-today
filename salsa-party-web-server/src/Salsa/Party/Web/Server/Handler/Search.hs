{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- This module contains all the routes that lead to a search results page.
--
-- The QueryR route parses query parameters into a QueryForm.
-- This QueryForm is then turned into SearchParameters.
-- (This step is necessary because FormInput m is not an Alternative.)
--
-- Using 'SearchParameters', we can then either show a results page directly
-- using 'searchResultsPage', or redirect to a more general /search page with
-- the query parameters filled in.
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
            <$> iopt latitudeField latitudeParameter
            <*> iopt longitudeField longitudeParameter
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

queryFormToSearchParameters :: QueryForm -> Handler SearchParameters
queryFormToSearchParameters QueryForm {..} = do
  searchParameterLocation <-
    case queryFormAddress of
      Just address -> pure $ SearchAddress address
      Nothing -> case queryFormCoordinates of
        Just coordinates -> pure $ SearchCoordinates coordinates
        Nothing -> invalidArgsI [MsgAddressOrCoordinates]
  let searchParameterBegin = case queryFormDay of
        Nothing -> BeginToday
        Just day -> BeginOn day
  pure SearchParameters {..}

getQueryR :: Handler Html
getQueryR = do
  searchParameters@SearchParameters {..} <- runInputGet queryForm >>= queryFormToSearchParameters
  case searchParameterLocation of
    SearchCoordinates _ -> searchResultsPage searchParameters
    SearchAddress address -> redirect $ case searchParameterBegin of
      BeginToday -> SearchR address
      BeginOn day -> SearchDayR address day

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

searchParametersQueryRoute :: (MonadHandler m, HandlerSite m ~ App) => SearchParameters -> m Text
searchParametersQueryRoute searchParameters = do
  urlRenderParams <- getUrlRenderParams
  pure $ urlRenderParams QueryR $ searchParameterParameters searchParameters

-- | Search parameters contain everything necessary to produce the html page of search results.
--
-- We do as much computation ahead of time, but not too much.
-- For example, the 'SearchBegin' parameter could be computed ahead of time, but then we would not be able to compute a nice title and description
data SearchParameters = SearchParameters
  { searchParameterLocation :: !SearchLocation,
    searchParameterBegin :: !SearchBegin
  }
  deriving (Show, Eq, Generic)

searchParameterParameters :: SearchParameters -> [(Text, Text)]
searchParameterParameters SearchParameters {..} =
  concat
    [ searchParameterLocationParameters searchParameterLocation,
      searchParameterBeginParameters searchParameterBegin
    ]

data SearchLocation
  = SearchAddress !Text
  | SearchCoordinates !Coordinates
  deriving (Show, Eq, Generic)

searchParameterLocationParameters :: SearchLocation -> [(Text, Text)]
searchParameterLocationParameters = \case
  SearchAddress address -> [(addressParameter, address)]
  SearchCoordinates Coordinates {..} ->
    [ (latitudeParameter, T.pack $ show coordinatesLat),
      (longitudeParameter, T.pack $ show coordinatesLon)
    ]

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

searchParameterBeginParameters :: SearchBegin -> [(Text, Text)]
searchParameterBeginParameters = \case
  BeginToday -> []
  BeginOn day -> [(dayParameter, T.pack $ formatTime defaultTimeLocale "%F" day)]

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
    searchParametersHtmlTitle searchParameters >>= setTitleI
    searchParametersHtmlDescription searchParameters >>= setDescriptionI
    title <- searchParametersHtmlTitle searchParameters
    timeLocale <- getTimeLocale
    prettyDayFormat <- getPrettyDayFormat
    let makeDayLink :: Day -> Widget
        makeDayLink day = do
          route <- searchParametersQueryRoute $ searchParameters {searchParameterBegin = BeginOn day}
          [whamlet|
            <a href=#{route}>
                #{formatTime timeLocale prettyDayFormat day}
                (_{autoDayMsg today day})
          |]
    if noData
      then $(widgetFile "search-no-results")
      else do
        let prevDay = addDays (negate daysAhead) begin
        let nextDay = addDays daysAhead begin
        let days = [begin .. end]
        prevDayRoute <- searchParametersQueryRoute $ searchParameters {searchParameterBegin = BeginOn prevDay}
        nextDayRoute <- searchParametersQueryRoute $ searchParameters {searchParameterBegin = BeginOn nextDay}
        let pagination = $(widgetFile "search-pagination")
        $(widgetFile "search")

searchParametersHtmlTitle :: SearchParameters -> WidgetFor App AppMessage
searchParametersHtmlTitle SearchParameters {..} = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  pure $ case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterBegin of
      BeginToday -> MsgSearchTitleAroundYourLocationToday
      BeginOn day -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress address -> case searchParameterBegin of
      BeginToday -> MsgSearchTitleAroundAddressToday address
      BeginOn day -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day

searchParametersHtmlDescription :: SearchParameters -> WidgetFor App AppMessage
searchParametersHtmlDescription SearchParameters {..} = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  pure $ case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterBegin of
      BeginToday -> MsgSearchDescriptionAroundYourLocationToday
      BeginOn day -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress address -> case searchParameterBegin of
      BeginToday -> MsgSearchDescriptionAroundAddressToday address
      BeginOn day -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day

searchParametersTitle :: SearchParameters -> AppMessage
searchParametersTitle SearchParameters {..} = case searchParameterLocation of
  SearchAddress address -> MsgSearchPartiesAroundAddress address
  SearchCoordinates _ -> MsgSearchPartiesAroundYourLocation
