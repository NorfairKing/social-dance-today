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
getSearchR placeQuery = do
  md <- lookupGetParam "day"
  day <- case md >>= parseTimeM True defaultTimeLocale "%F" . T.unpack of
    Nothing -> liftIO $ utctDay <$> getCurrentTime -- today
    Just d -> pure d
  Entity _ place <- lookupPlace placeQuery
  parties <- runDB $ searchQuery day place
  withNavBar $(widgetFile "search")

searchQuery :: MonadIO m => Day -> Place -> SqlPersistT m [Entity Party]
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
      pure party
