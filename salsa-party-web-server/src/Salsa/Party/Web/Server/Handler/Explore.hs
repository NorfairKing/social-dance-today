{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Explore where

import Data.FileEmbed
import Data.List
import Data.Ord
import qualified Data.Text as T
import Language.Haskell.TH.Syntax as TH
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Query
import System.Directory

getExploreR :: Handler Html
getExploreR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  locationTups <- fmap (sortOn (Down . snd) . catMaybes) $
    runDB $
      forM locations $ \Location {..} -> do
        nbUpcomingParties <- explorePartiesAroundLocationQuery today (placeCoordinates locationPlace)
        pure $ do
          guard $ nbUpcomingParties > 0
          pure (locationPlace, nbUpcomingParties)

  withNavBar $ do
    setTitleI MsgExploreTitle
    setDescriptionI MsgExploreDescription
    $(widgetFile "explore")

getExploreSkylineR :: Text -> Handler TypedContent
getExploreSkylineR locationName = do
  let locationsDir = $(makeRelativeToProject "static/locations/" >>= TH.lift)
  let filepath = locationsDir <> T.unpack locationName <> ".jpg"
  exists <- liftIO $ doesFileExist filepath
  if exists
    then do
      sendFile "image/jpeg" filepath
    else notFound

-- TODO we can probably optimise this with a count query, or at least we don't have to fetch any posters.
explorePartiesAroundLocationQuery :: MonadIO m => Day -> Coordinates -> SqlPersistT m Int
explorePartiesAroundLocationQuery today coordinates =
  countSearchResults
    <$> searchQuery
      today
      Nothing
      coordinates
