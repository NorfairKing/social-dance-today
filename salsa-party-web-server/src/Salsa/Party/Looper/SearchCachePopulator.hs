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
import Salsa.Party.Web.Server.Constants (defaultDaysAhead, development)
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.Search.Types

runSearchCachePopulator :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runSearchCachePopulator = do
  today <- liftIO $ utctDay <$> getCurrentTime
  forM_ locations $ \location -> do
    let place = locationPlace location
    let address = placeQuery place
    let coordinates = placeCoordinates place

    populateCacheForQuery
      address
      SearchQuery
        { searchQueryBegin = today,
          searchQueryMEnd = Just $ addDays (defaultDaysAhead - 1) today,
          searchQueryCoordinates = coordinates,
          searchQueryDistance = Just defaultMaximumDistance,
          searchQueryDanceStyle = Nothing
        }

  -- Purge expired results, because that doesn't happen automatically.
  searchResultCache <- asks appSearchResultCache
  liftIO $ Cache.purgeExpired searchResultCache

populateCacheForQuery :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Text -> SearchQuery -> m ()
populateCacheForQuery address query = do
  searchResultCache <- asks appSearchResultCache
  mResult <- liftIO $ Cache.lookup searchResultCache query
  case mResult of
    Nothing ->
      logDebugN $
        T.pack $
          unlines
            [ "No results for this query yet, populate it asap:",
              ppShow query
            ]
    Just _ ->
      when (not development) $ do
        logDebugN $
          T.pack $
            unlines
              [ "Waiting a bit to populate the search cache for this query to not overload the server with the sudden amount of queries:",
                ppShow query
              ]
        liftIO $ threadDelay 5_000_000

  logInfoN $
    T.pack $
      unwords
        [ "Populating search cache for location",
          show address
        ]
  pool <- asks appConnectionPool
  let runDBHere func = runSqlPool (retryOnBusy func) pool
  queryResults <- runDBHere $ runUncachedSearchQueryForResults query

  logDebugN $
    T.pack $
      unlines
        [ "Inserting search results for query:",
          ppShow query,
          ppShow queryResults
        ]
  liftIO $ Cache.insert' searchResultCache Nothing query queryResults
