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

getAdvancedSearchR :: Handler Html
getAdvancedSearchR = withNavBar $ do
  let queryId = "query"
  let statusId = "status"
  let helpId = "help"
  let minDistance = mToKm minimumMaximumDistance
  let maxDistance = mToKm maximumMaximumDistance
  let defaultDistance = mToKm defaultMaximumDistance
  let stepDistance = mToKm maximumDistanceStep
  setTitleI MsgAdvancedSearchTitle
  setDescriptionI MsgAdvancedSearchDescription
  $(widgetFile "advanced-search") <> locateMeButton queryId statusId helpId

mToKm :: Word -> Word
mToKm = (`div` 1000)

kmToM :: Word -> Word
kmToM = (* 1000)

data QueryForm = QueryForm
  { queryFormAddress :: Maybe Text,
    queryFormCoordinates :: Maybe Coordinates,
    queryFormBegin :: Maybe Day,
    queryFormOn :: Maybe Day,
    queryFormDistance :: Maybe Word
  }
  deriving (Show, Eq, Generic)

addressParameter :: Text
addressParameter = "address"

latitudeParameter :: Text
latitudeParameter = "latitude"

longitudeParameter :: Text
longitudeParameter = "longitude"

onParameter :: Text
onParameter = "on"

beginParameter :: Text
beginParameter = "begin"

distanceParameter :: Text
distanceParameter = "distance"

queryForm :: FormInput Handler QueryForm
queryForm =
  QueryForm
    <$> iopt textField addressParameter
    <*> ( liftA2 Coordinates
            <$> iopt latitudeField latitudeParameter
            <*> iopt longitudeField longitudeParameter
        )
    <*> iopt dayField beginParameter
    <*> iopt dayField onParameter
    <*> iopt intField distanceParameter

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
  let searchParameterDate = case queryFormOn of
        Just day -> SearchExactlyOn day
        Nothing -> case queryFormBegin of
          Just day -> SearchFromOn day
          Nothing -> SearchFromToday
  let searchParameterDistance = kmToM <$> queryFormDistance
  pure SearchParameters {..}

getQueryR :: Handler Html
getQueryR = do
  searchParameters@SearchParameters {..} <- runInputGet queryForm >>= queryFormToSearchParameters
  case searchParameterDistance of
    Just _ -> searchResultsPage searchParameters
    Nothing ->
      case searchParameterLocation of
        SearchCoordinates _ -> searchResultsPage searchParameters
        SearchAddress address -> case searchParameterDate of
          SearchFromToday -> redirect $ SearchR address
          SearchFromOn _ -> searchResultsPage searchParameters
          SearchExactlyOn day -> redirect $ SearchDayR address day

getSearchR :: Text -> Handler Html
getSearchR query = do
  searchResultsPage
    SearchParameters
      { searchParameterLocation = SearchAddress query,
        searchParameterDate = SearchFromToday,
        searchParameterDistance = Nothing
      }

getSearchDayR :: Text -> Day -> Handler Html
getSearchDayR query day =
  searchResultsPage
    SearchParameters
      { searchParameterLocation = SearchAddress query,
        searchParameterDate = SearchExactlyOn day,
        searchParameterDistance = Nothing
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
    searchParameterDate :: !SearchDate,
    searchParameterDistance :: !(Maybe Word)
  }
  deriving (Show, Eq, Generic)

searchParameterParameters :: SearchParameters -> [(Text, Text)]
searchParameterParameters SearchParameters {..} =
  concat
    [ searchParameterLocationParameters searchParameterLocation,
      searchParameterDateParameters searchParameterDate,
      [(distanceParameter, T.pack $ show dist) | dist <- maybeToList searchParameterDistance]
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

data SearchDate
  = -- | From today until defaultDaysAhead days ahead
    SearchFromToday
  | -- | From this day until defaultDaysAhead days ahead
    SearchFromOn !Day
  | -- | Exactly on this day, no days ahead
    SearchExactlyOn !Day
  deriving (Show, Eq, Generic)

searchParameterDateParameters :: SearchDate -> [(Text, Text)]
searchParameterDateParameters = \case
  SearchFromToday -> []
  SearchFromOn day -> [(beginParameter, T.pack $ formatTime defaultTimeLocale "%F" day)]
  SearchExactlyOn day -> [(onParameter, T.pack $ formatTime defaultTimeLocale "%F" day)]

searchResultsPage :: SearchParameters -> Handler Html
searchResultsPage searchParameters@SearchParameters {..} = do
  today <- liftIO $ utctDay <$> getCurrentTime -- today

  -- Resolve begin day
  let begin = case searchParameterDate of
        SearchFromToday -> today
        SearchFromOn day -> day
        SearchExactlyOn day -> day

  -- Resolve end day
  let end = case searchParameterDate of
        SearchFromToday -> addDays (defaultDaysAhead -1) today
        SearchFromOn day -> addDays (defaultDaysAhead -1) day
        SearchExactlyOn day -> day

  -- Resolve coordinates
  coordinates <- resolveSearchLocation searchParameterLocation

  -- Do the actual search
  searchResults <-
    runDB $
      runSearchQuery
        SearchQuery
          { searchQueryBegin = begin,
            searchQueryMEnd = Just end,
            searchQueryCoordinates = coordinates,
            searchQueryDistance = Just $ fromMaybe defaultMaximumDistance searchParameterDistance
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
          route <- searchParametersQueryRoute $ searchParameters {searchParameterDate = SearchExactlyOn day}
          [whamlet|
            <a href=#{route}>
                #{formatTime timeLocale prettyDayFormat day}
                (_{autoDayMsg today day})
          |]
    if noData
      then $(widgetFile "search-no-results")
      else do
        let prevDate = case searchParameterDate of
              SearchFromToday -> SearchFromOn $ addDays (negate defaultDaysAhead) today
              SearchFromOn day -> SearchFromOn $ addDays (negate defaultDaysAhead) day
              SearchExactlyOn day -> SearchExactlyOn $ addDays (-1) day
        let nextDate = case searchParameterDate of
              SearchFromToday -> SearchFromOn $ addDays defaultDaysAhead today
              SearchFromOn day -> SearchFromOn $ addDays defaultDaysAhead day
              SearchExactlyOn day -> SearchExactlyOn $ addDays 1 day
        let days = [begin .. end]
        prevDayRoute <- searchParametersQueryRoute $ searchParameters {searchParameterDate = prevDate}
        nextDayRoute <- searchParametersQueryRoute $ searchParameters {searchParameterDate = nextDate}
        let pagination = $(widgetFile "search-pagination")
        $(widgetFile "search")

searchParametersHtmlTitle :: SearchParameters -> WidgetFor App AppMessage
searchParametersHtmlTitle SearchParameters {..} = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  pure $ case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterDate of
      SearchFromToday -> MsgSearchTitleAroundYourLocationToday
      SearchFromOn day -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
      SearchExactlyOn day -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress address -> case searchParameterDate of
      SearchFromToday -> MsgSearchTitleAroundAddressToday address
      SearchFromOn day -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day
      SearchExactlyOn day -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day

searchParametersHtmlDescription :: SearchParameters -> WidgetFor App AppMessage
searchParametersHtmlDescription SearchParameters {..} = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  pure $ case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterDate of
      SearchFromToday -> MsgSearchDescriptionAroundYourLocationToday
      SearchFromOn day -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
      SearchExactlyOn day -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress address -> case searchParameterDate of
      SearchFromToday -> MsgSearchDescriptionAroundAddressToday address
      SearchFromOn day -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day
      SearchExactlyOn day -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day

searchParametersTitle :: SearchParameters -> AppMessage
searchParametersTitle SearchParameters {..} = case searchParameterLocation of
  SearchAddress address -> MsgSearchPartiesAroundAddress address
  SearchCoordinates _ -> MsgSearchPartiesAroundYourLocation

defaultDaysAhead :: Integer
defaultDaysAhead = 7
