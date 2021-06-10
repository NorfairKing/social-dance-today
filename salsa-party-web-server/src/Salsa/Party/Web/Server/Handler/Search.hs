{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getQueryR :: Handler Html
getQueryR = do
  placeQuery <- runInputGet $ ireq textField "query"
  redirect $ SearchR placeQuery

-- TODO make this admin only?
getPlaceR :: Text -> Handler Html
getPlaceR placeQuery = do
  Entity _ place <- lookupPlace placeQuery
  withNavBar $(widgetFile "place")

getSearchR :: Text -> Handler Html
getSearchR query = do
  md <- lookupGetParam "day"
  let mDay = md >>= parseTimeM True defaultTimeLocale "%F" . T.unpack
  day <- case mDay of
    Nothing -> liftIO $ utctDay <$> getCurrentTime -- today
    Just d -> pure d
  Entity _ place <- lookupPlace query
  parties <- runDB $ searchQuery day place
  withNavBar $(widgetFile "search")

-- For a given day and a given place,
-- find all parties sorted by distance.
searchQuery :: MonadIO m => Day -> Place -> SqlPersistT m [(Entity Party, Entity Place)]
searchQuery day place =
  E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyDay E.==. E.val day)
      let latDiff = p E.^. PlaceLat E.-. E.val (placeLat place)
      let lonDiff = p E.^. PlaceLon E.-. E.val (placeLon place)
      let latDiffSquared = latDiff E.*. latDiff
      let lonDiffSquared = lonDiff E.*. lonDiff
      -- Luckily the square function is monotone so we don't need to sqrt here
      let distSquared = latDiffSquared E.+. lonDiffSquared
      E.orderBy [E.asc distSquared]
      pure (party, p)
