{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Looper.ExploreCachePopulator
  ( runExploreCachePopulator,
  )
where

import qualified Data.Cache as Cache
import qualified Data.Text as T
import Salsa.Party.DB.Migration
import Salsa.Party.Looper.Import
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Handler.Explore

runExploreCachePopulator :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runExploreCachePopulator = do
  pool <- asks appConnectionPool
  today <- liftIO $ utctDay <$> getCurrentTime
  exploreResultCache <- asks appExploreResultCache
  let runDBHere func = runSqlPool (retryOnBusy func) pool
  forM_ locations $ \location -> do
    let placeName = placeQuery $ locationPlace location
    let coordinates = placeCoordinates $ locationPlace location
    mResult <- liftIO $ Cache.lookup exploreResultCache coordinates
    case mResult of
      Nothing ->
        logDebugN $
          T.pack $
            unwords
              [ "No results in search cache for this place yet, populate it asap:",
                show placeName
              ]
      Just _ -> do
        when (not development) $ do
          logDebugN $
            T.pack $
              unwords
                [ "Waiting a bit to populate the explore cache for",
                  "to not overload the server with the sudden amount of queries"
                ]
          liftIO $ threadDelay 5_000_000

    logInfoN $ T.pack $ unwords ["Populating explore cache for", show placeName]
    exploreQueryResult <- runDBHere $ uncachedExplorePartiesAroundLocationQuery today coordinates
    liftIO $ Cache.insert' exploreResultCache Nothing coordinates exploreQueryResult

  -- Purge expired results, because that doesn't happen automatically.
  liftIO $ Cache.purgeExpired exploreResultCache
