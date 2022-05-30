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
import Salsa.Party.Web.Server.Handler.Explore
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.Search.Types

runSearchCachePopulator :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runSearchCachePopulator = do
  pool <- asks appConnectionPool
  today <- liftIO $ utctDay <$> getCurrentTime
  searchResultCache <- asks appSearchResultCache
  let runDBHere func = runSqlPool (retryOnBusy func) pool
  forM_ locations $ \location -> do
    let placeName = placeQuery $ locationPlace location
    logDebugN $
      T.pack $
        unwords
          [ "Waiting a bit to populate the search cache for",
            "to not overload the server with the sudden amount of queries"
          ]
    liftIO $ threadDelay 5_000_000
    logInfoN $ T.pack $ unwords ["Populating search cache for", show placeName]
    let coordinates = placeCoordinates $ locationPlace location

    logDebugN $ T.pack $ unwords ["Populating explore search cache for", show placeName]
    let exploreQuery = exploreQueryFor today coordinates
    exploreQueryResults <- runDBHere $ runUncachedSearchQueryForResults exploreQuery
    liftIO $ Cache.insert' searchResultCache (Just searchResultCacheTimeSpec) exploreQuery exploreQueryResults

    logDebugN $ T.pack $ unwords ["Populating normal user search cache for", show placeName]
    let userQuery =
          SearchQuery
            { searchQueryBegin = today,
              searchQueryMEnd = Just $ addDays (defaultDaysAhead - 1) today,
              searchQueryCoordinates = coordinates,
              searchQueryDistance = Just defaultMaximumDistance
            }
    userQueryResults <- runDBHere $ runUncachedSearchQueryForResults userQuery
    liftIO $ Cache.insert' searchResultCache (Just searchResultCacheTimeSpec) userQuery userQueryResults

    pure () -- TODO
