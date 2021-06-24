{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

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
