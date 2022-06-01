{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Looper.SearchCachePopulator
  ( runSearchCachePopulator,
  )
where

import qualified Data.Cache as Cache
import qualified Data.Text as T
import Salsa.Party.DB.Migration
import Salsa.Party.Looper.Import
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Handler.Explore
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.Search.Types

runSearchCachePopulator :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runSearchCachePopulator = do
  pool <- asks appConnectionPool
  today <- liftIO $ utctDay <$> getCurrentTime
  searchResultCache <- asks appSearchResultCache
  exploreResultCache <- asks appExploreResultCache
  let runDBHere func = runSqlPool (retryOnBusy func) pool
  forM_ locations $ \location -> do
    when (not development) $ do
      logDebugN $
        T.pack $
          unwords
            [ "Waiting a bit to populate the search cache for",
              "to not overload the server with the sudden amount of queries"
            ]
      liftIO $ threadDelay 5_000_000
    let placeName = placeQuery $ locationPlace location
    let coordinates = placeCoordinates $ locationPlace location
    logInfoN $ T.pack $ unwords ["Populating search cache for", show placeName]
    let userQuery =
          SearchQuery
            { searchQueryBegin = today,
              searchQueryMEnd = Just $ addDays (defaultDaysAhead - 1) today,
              searchQueryCoordinates = coordinates,
              searchQueryDistance = Just defaultMaximumDistance
            }
    userQueryResults <- runDBHere $ runUncachedSearchQueryForResults userQuery
    liftIO $ Cache.insert' searchResultCache (Just searchResultCacheTimeSpec) userQuery userQueryResults

    logInfoN $ T.pack $ unwords ["Populating explore cache for", show placeName]
    exploreQueryResult <- runDBHere $ uncachedExplorePartiesAroundLocationQuery today coordinates
    liftIO $ Cache.insert' exploreResultCache (Just searchResultCacheTimeSpec) coordinates exploreQueryResult
