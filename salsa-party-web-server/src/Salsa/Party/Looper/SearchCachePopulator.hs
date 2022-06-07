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
    let coordinates = placeCoordinates $ locationPlace location
    let query =
          SearchQuery
            { searchQueryBegin = today,
              searchQueryMEnd = Just $ addDays (defaultDaysAhead - 1) today,
              searchQueryCoordinates = coordinates,
              searchQueryDistance = Just defaultMaximumDistance
            }
    mResult <- liftIO $ Cache.lookup searchResultCache query
    case mResult of
      Nothing ->
        logDebugN $
          T.pack $
            unwords
              [ "No results in search cache for this place yet, populate it asap:",
                show placeName
              ]
      Just _ ->
        when (not development) $ do
          logDebugN $
            T.pack $
              unwords
                [ "Waiting a bit to populate the search cache for",
                  show placeName,
                  "to not overload the server with the sudden amount of queries"
                ]
          liftIO $ threadDelay 5_000_000

    logInfoN $ T.pack $ unwords ["Populating search cache for", show placeName]
    queryResults <- runDBHere $ runUncachedSearchQueryForResults query
    liftIO $ Cache.insert' searchResultCache Nothing query queryResults
