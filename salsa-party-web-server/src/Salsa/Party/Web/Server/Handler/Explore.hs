{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Explore where

import Control.Monad.Logger
import Data.Hashable
import Data.List
import Data.Ord
import qualified Data.Text as T
import Path
import Path.IO
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Query

getExploreR :: Handler Html
getExploreR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  locationTups <- fmap (sortOn (Down . snd) . catMaybes) $
    runDB $
      forM locations $ \Location {..} -> do
        nbUpcomingParties <- explorePartiesAroundLocationQuery today (placeCoordinates locationPlace)
        pure $ do
          guard $ nbUpcomingParties > minimumUpcomingParties
          pure (locationPlace, nbUpcomingParties)

  withNavBar $ do
    setTitleI MsgExploreTitle
    setDescriptionI MsgExploreDescription
    $(widgetFile "explore")

minimumUpcomingParties :: Int
minimumUpcomingParties = 10

-- TODO we can probably optimise this with a count query, or at least we don't have to fetch any posters.
explorePartiesAroundLocationQuery :: MonadIO m => Day -> Coordinates -> SqlPersistT m Int
explorePartiesAroundLocationQuery today coordinates =
  countSearchResults
    <$> searchQuery
      today
      Nothing
      coordinates

getExploreSkylineR :: Text -> Handler TypedContent
getExploreSkylineR locationName = do
  staticDir <- getsYesod appStaticDir
  locationsDir <- resolveDir staticDir "locations"
  -- To make sure that the file is definitely in this dir and we don't leak filesystem access
  case parseRelFile (T.unpack locationName <> ".jpg") of
    Nothing -> notFound
    Just relFile -> do
      -- Cache forever, they're just skyline images, who cares.
      neverExpires
      let filepath = locationsDir </> relFile
      logDebugN $ T.pack $ unwords ["Skyline file for location", show locationName <> ":", fromAbsFile filepath]
      exists <- liftIO $ doesFileExist filepath
      if exists
        then sendFile "image/jpeg" $ fromAbsFile filepath
        else
          let stockImage = stockSkylineImages !! (abs (hash filepath) `mod` length stockSkylineImages)
           in sendImageOrNotFound $ locationsDir </> stockImage

stockSkylineImages :: [Path Rel File]
stockSkylineImages =
  [ [relfile|stock-night-time-skyline-1.jpg|],
    [relfile|stock-night-time-skyline-2.jpg|],
    [relfile|stock-night-time-skyline-3.jpg|]
  ]

sendImageOrNotFound :: Path Abs File -> Handler TypedContent
sendImageOrNotFound filepath = do
  exists <- liftIO $ doesFileExist filepath
  if exists
    then sendFile "image/jpeg" $ fromAbsFile filepath
    else notFound
