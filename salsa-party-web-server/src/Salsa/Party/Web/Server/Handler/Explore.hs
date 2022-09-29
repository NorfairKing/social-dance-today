{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Explore where

import Control.Monad.Logger
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Hashable
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Database.Esqueleto.Legacy as E
import Path
import Path.IO
import Safe
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Query

getExploreR :: Handler Html
getExploreR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  exploreResultCache <- getsYesod appExploreResultCache
  locationTups <- fmap (sortOn (Down . snd) . catMaybes) $
    runDB $
      forM locations $ \Location {..} -> do
        nbUpcomingParties <- explorePartiesAroundLocationQuery exploreResultCache today (placeCoordinates locationPlace)
        pure $ do
          guard $ nbUpcomingParties > minimumUpcomingParties
          pure (locationPlace, nbUpcomingParties)

  withNavBar $ do
    setTitleI MsgExploreTitle
    setDescriptionI MsgExploreDescription
    addStylesheet $ StaticR zoom_without_container_css
    $(widgetFile "explore")

minimumUpcomingParties :: Word
minimumUpcomingParties = 10

explorePartiesAroundLocationQuery :: (MonadIO m, MonadLogger m) => Cache Coordinates Word -> Day -> Coordinates -> SqlPersistT m Word
explorePartiesAroundLocationQuery exploreResultCache today coordinates = do
  mCachedResult <- liftIO $ Cache.lookup exploreResultCache coordinates
  case mCachedResult of
    Just cachedResults -> do
      logDebugN $
        T.pack $
          unlines
            [ "Found cached explore result, not doing count.",
              show coordinates
            ]
      pure cachedResults
    Nothing -> do
      logDebugN $
        T.pack $
          unlines
            [ "No cached explore result, doing count.",
              show coordinates
            ]
      result <- uncachedExplorePartiesAroundLocationQuery today coordinates
      liftIO $ Cache.insert' exploreResultCache Nothing coordinates result
      pure result

uncachedExplorePartiesAroundLocationQuery :: MonadIO m => Day -> Coordinates -> SqlPersistT m Word
uncachedExplorePartiesAroundLocationQuery today coordinates = do
  let distance = defaultMaximumDistance
  rawPartyPlaces <- E.select $
    E.from $ \((party `E.InnerJoin` place)) -> do
      E.on (party E.^. PartyPlace E.==. place E.^. PlaceId)
      E.where_ $ dayLimit (party E.^. PartyDay) today Nothing
      distanceEstimationQuery distance coordinates place
      pure place

  -- Overcounting a bit because we're not deduplicating external events
  -- but that's fine for the explore page
  rawExternalEventPlaces <- E.select $
    E.from $ \(externalEvent `E.InnerJoin` place) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
      E.where_ $ dayLimit (externalEvent E.^. ExternalEventDay) today Nothing
      distanceEstimationQuery distance coordinates place
      pure place

  let places = map entityVal $ rawPartyPlaces ++ rawExternalEventPlaces

  pure $
    fromIntegral $
      length $
        filter
          (\place -> coordinates `distanceTo` placeCoordinates place <= defaultMaximumDistance)
          places

getExploreSkylineR :: Text -> Handler TypedContent
getExploreSkylineR locationName = do
  locationsDir <- resolveDir staticDir "locations"
  -- To make sure that the file is definitely in this dir and we don't leak filesystem access
  case parseRelFile (T.unpack locationName <> ".jpg") of
    Nothing -> notFound
    Just relFile -> do
      -- Cache forever, they're just skyline images, who cares.
      neverExpires
      let filepath = locationsDir </> relFile
      logDebugN $ T.pack $ unwords ["Skyline file for location", show locationName <> ":", fromAbsFile filepath]
      fileExists <- liftIO $ doesFileExist filepath
      if fileExists
        then sendFile "image/jpeg" $ fromAbsFile filepath
        else case stockSkylineImages `atMay` (abs (hash filepath) `mod` length stockSkylineImages) of
          Nothing -> notFound
          Just stockImage -> sendImageOrNotFound $ locationsDir </> stockImage

stockSkylineImages :: [Path Rel File]
stockSkylineImages =
  [ [relfile|stock-night-time-skyline-1.jpg|],
    [relfile|stock-night-time-skyline-2.jpg|],
    [relfile|stock-night-time-skyline-3.jpg|]
  ]

sendImageOrNotFound :: Path Abs File -> Handler TypedContent
sendImageOrNotFound filepath = do
  imageExists <- liftIO $ doesFileExist filepath
  if imageExists
    then sendFile "image/jpeg" $ fromAbsFile filepath
    else notFound
