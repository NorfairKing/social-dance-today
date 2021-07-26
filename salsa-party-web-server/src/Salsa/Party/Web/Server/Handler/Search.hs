{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

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

queryForm :: FormInput Handler QueryForm
queryForm =
  QueryForm
    <$> iopt textField "address"
    <*> ( liftA2 Coordinates
            <$> (fmap realToFrac <$> iopt doubleField "latitude")
            <*> (fmap realToFrac <$> iopt doubleField "longitude")
        )
    <*> iopt dayField "day"

postQueryR :: Handler Html
postQueryR = do
  QueryForm {..} <- runInputPost queryForm
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
  let daysAhead = 7
  let begin = day
  let end = addDays (daysAhead - 1) begin
      prevDay = addDays (negate daysAhead) begin
      nextDay = addDays daysAhead begin
      days = [begin .. end]
  let toDouble :: Nano -> Double
      toDouble = realToFrac
  searchResults <- runDB $ searchQuery begin (Just end) coordinates
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat

  let title = case (mAddress, mDay) of
        (Nothing, Nothing) -> MsgSearchTitleAroundYourLocationToday
        (Nothing, Just d) -> MsgSearchTitleAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat d
        (Just address, Nothing) -> MsgSearchTitleAroundAddressToday address
        (Just address, Just d) -> MsgSearchTitleAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat d

  let description = case (mAddress, mDay) of
        (Nothing, Nothing) -> MsgSearchDescriptionAroundYourLocationToday
        (Nothing, Just d) -> MsgSearchDescriptionAroundYourLocationOnDay $ formatTime timeLocale prettyDayFormat d
        (Just address, Nothing) -> MsgSearchDescriptionAroundAddressToday address
        (Just address, Just d) -> MsgSearchDescriptionAroundAddressOnDay address $ formatTime timeLocale prettyDayFormat d

  noData <-
    if nullSearchResults searchResults
      then runDB $ noDataQuery coordinates
      else pure False
  if noData
    then do
      withNavBar $ do
        setTitleI title
        setDescriptionI description
        $(widgetFile "search-no-results")
    else do
      withNavBar $ do
        setTitleI title
        setDescriptionI description
        let pagination = $(widgetFile "search-pagination")
        $(widgetFile "search")
