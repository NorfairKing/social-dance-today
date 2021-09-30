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
  { queryFormAddress :: !(Maybe Text),
    queryFormCoordinates :: !(Maybe Coordinates),
    queryFormBegin :: !(Maybe Day),
    queryFormEnd :: !(Maybe Day),
    queryFormOn :: !(Maybe Day),
    queryFormDistance :: !(Maybe Word)
  }
  deriving (Show, Eq, Generic)

addressParameter :: Text
addressParameter = "address"

latitudeParameter :: Text
latitudeParameter = "latitude"

longitudeParameter :: Text
longitudeParameter = "longitude"

beginParameter :: Text
beginParameter = "begin"

endParameter :: Text
endParameter = "end"

onParameter :: Text
onParameter = "on"

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
    <*> iopt dayField endParameter
    <*> iopt dayField onParameter
    <*> iopt distanceField distanceParameter

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

distanceField :: Field Handler Word
distanceField =
  checkM
    ( \d ->
        pure $
          let ms = kmToM d
           in if ms > maximumMaximumDistance
                then Left (MsgDistanceTooGreat (show d))
                else Right ms
    )
    intField

queryFormToSearchParameters :: QueryForm -> Handler SearchParameters
queryFormToSearchParameters QueryForm {..} = do
  searchParameterLocation <-
    case queryFormAddress of
      Just address -> pure $ SearchAddress address
      Nothing -> case queryFormCoordinates of
        Just coordinates -> pure $ SearchCoordinates coordinates
        Nothing -> invalidArgsI [MsgAddressOrCoordinates]
  searchParameterDate <- case queryFormOn of
    Just day -> pure $ SearchExactlyOn day
    Nothing -> case queryFormBegin of
      Nothing -> pure SearchFromToday
      Just begin -> case queryFormEnd of
        Nothing -> pure $ SearchFromOn begin
        Just end ->
          let d = diffDays end begin
           in if d < 0
                then invalidArgsI [MsgEndBeforeBegin]
                else
                  if d > maximumDaysAhead
                    then invalidArgsI [MsgEndTooFarAhead]
                    else pure $ SearchFromTo begin end
  let searchParameterDistance = queryFormDistance
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
          SearchExactlyOn day -> redirect $ SearchDayR address day
          SearchFromTo begin end -> redirect $ SearchFromToR address begin end
          SearchFromOn _ -> searchResultsPage searchParameters

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

getSearchFromToR :: Text -> Day -> Day -> Handler Html
getSearchFromToR query begin end =
  searchResultsPage
    SearchParameters
      { searchParameterLocation = SearchAddress query,
        searchParameterDate = SearchFromTo begin end,
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
    searchParameterDistance :: !(Maybe Word) -- Nothing means unspecified
  }
  deriving (Show, Eq, Generic)

searchParameterParameters :: SearchParameters -> [(Text, Text)]
searchParameterParameters SearchParameters {..} =
  concat
    [ searchParameterLocationParameters searchParameterLocation,
      searchParameterDateParameters searchParameterDate,
      [(distanceParameter, T.pack $ show $ mToKm dist) | dist <- maybeToList searchParameterDistance]
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
  | -- | Between these two days
    SearchFromTo !Day !Day
  | -- | Exactly on this day, no days ahead
    SearchExactlyOn !Day
  deriving (Show, Eq, Generic)

searchParameterDateParameters :: SearchDate -> [(Text, Text)]
searchParameterDateParameters = \case
  SearchFromToday -> []
  SearchExactlyOn day -> [(onParameter, T.pack $ formatTime defaultTimeLocale "%F" day)]
  SearchFromOn day -> [(beginParameter, T.pack $ formatTime defaultTimeLocale "%F" day)]
  SearchFromTo begin end ->
    [ (beginParameter, T.pack $ formatTime defaultTimeLocale "%F" begin),
      (endParameter, T.pack $ formatTime defaultTimeLocale "%F" end)
    ]

searchResultsPage :: SearchParameters -> Handler Html
searchResultsPage searchParameters@SearchParameters {..} = do
  today <- liftIO $ utctDay <$> getCurrentTime -- today

  -- Resolve begin day
  let begin = case searchParameterDate of
        SearchFromToday -> today
        SearchFromOn day -> day
        SearchFromTo day _ -> day
        SearchExactlyOn day -> day

  -- Resolve end day
  let end = case searchParameterDate of
        SearchFromToday -> addDays (defaultDaysAhead -1) today
        SearchFromOn day -> addDays (defaultDaysAhead -1) day
        SearchFromTo _ day -> day
        SearchExactlyOn day -> day

  -- Resolve coordinates
  coordinates <- resolveSearchLocation searchParameterLocation

  -- Build the query
  let query =
        SearchQuery
          { searchQueryBegin = begin,
            searchQueryMEnd = Just end,
            searchQueryCoordinates = coordinates,
            searchQueryDistance = Just $ fromMaybe defaultMaximumDistance searchParameterDistance
          }

  -- Do the actual search
  searchResult <- runDB $ runSearchQuery query

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
    case searchResult of
      NoDataYet -> $(widgetFile "search-no-results")
      ResultsFound searchResults -> do
        let robotsTags =
              catMaybes
                [ Just "noarchive",
                  case searchParameterDate of
                    SearchFromToday -> Nothing -- Stays good
                    _ -> Just $ T.pack $ "unavailable_after: " <> formatTime defaultTimeLocale "%F" (addDays daysToKeepParties end)
                ]
        toWidgetHead [hamlet|<meta property="robots" content=#{T.intercalate ", " robotsTags}>|]
        let days = [begin .. end]
        prevDayRoute <- searchParametersQueryRoute $ searchParameters {searchParameterDate = navPrevSearchDate today searchParameterDate}
        nextDayRoute <- searchParametersQueryRoute $ searchParameters {searchParameterDate = navNextSearchDate today searchParameterDate}
        let pagination = $(widgetFile "search-pagination")
        $(widgetFile "search")

navPrevSearchDate :: Day -> SearchDate -> SearchDate
navPrevSearchDate today = \case
  SearchFromToday -> SearchFromOn $ addDays (negate defaultDaysAhead) today
  SearchFromOn day -> SearchFromOn $ addDays (negate defaultDaysAhead) day
  SearchFromTo b e -> SearchFromTo (addDays (diffDays b e) b) (addDays (-1) b)
  SearchExactlyOn day -> SearchExactlyOn $ addDays (-1) day

navNextSearchDate :: Day -> SearchDate -> SearchDate
navNextSearchDate today = \case
  SearchFromToday -> SearchFromOn $ addDays defaultDaysAhead today
  SearchFromOn day -> SearchFromOn $ addDays defaultDaysAhead day
  SearchFromTo b e -> SearchFromTo (addDays 1 e) (addDays (diffDays e b) e)
  SearchExactlyOn day -> SearchExactlyOn $ addDays 1 day

searchParametersHtmlTitle :: SearchParameters -> WidgetFor App AppMessage
searchParametersHtmlTitle SearchParameters {..} = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  pure $ case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterDate of
      SearchFromToday -> MsgSearchTitleAroundYourLocationToday
      SearchFromOn day -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
      SearchFromTo begin end -> MsgSearchTitleAroundYourLocationFromTo (formatTime timeLocale prettyDayFormat begin) (formatTime timeLocale prettyDayFormat end)
      SearchExactlyOn day -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress address -> case searchParameterDate of
      SearchFromToday -> MsgSearchTitleAroundAddressToday address
      SearchFromOn day -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day
      SearchFromTo begin end -> MsgSearchTitleAroundAddressFromTo address (formatTime timeLocale prettyDayFormat begin) (formatTime timeLocale prettyDayFormat end)
      SearchExactlyOn day -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day

searchParametersHtmlDescription :: SearchParameters -> WidgetFor App AppMessage
searchParametersHtmlDescription SearchParameters {..} = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  pure $ case searchParameterLocation of
    SearchCoordinates _ -> case searchParameterDate of
      SearchFromToday -> MsgSearchDescriptionAroundYourLocationToday
      SearchFromOn day -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
      SearchFromTo begin end -> MsgSearchDescriptionAroundYourLocationFromTo (formatTime timeLocale prettyDayFormat begin) (formatTime timeLocale prettyDayFormat end)
      SearchExactlyOn day -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat day
    SearchAddress address -> case searchParameterDate of
      SearchFromToday -> MsgSearchDescriptionAroundAddressToday address
      SearchFromOn day -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day
      SearchFromTo begin end -> MsgSearchDescriptionAroundAddressFromTo address (formatTime timeLocale prettyDayFormat begin) (formatTime timeLocale prettyDayFormat end)
      SearchExactlyOn day -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat day

searchParametersTitle :: SearchParameters -> AppMessage
searchParametersTitle SearchParameters {..} = case searchParameterLocation of
  SearchAddress address -> MsgSearchPartiesAroundAddress address
  SearchCoordinates _ -> MsgSearchPartiesAroundYourLocation

defaultDaysAhead :: Integer
defaultDaysAhead = 7

maximumDaysAhead :: Integer
maximumDaysAhead = 5 * defaultDaysAhead

daysToKeepParties :: Integer
daysToKeepParties = 60
