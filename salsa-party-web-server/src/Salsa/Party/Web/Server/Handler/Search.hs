{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

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
  Entity _ place <- lookupPlace placeQuery
  today <- liftIO $ utctDay <$> getCurrentTime
  parties <- runDB $
    E.select $
      E.from $ \(party `E.InnerJoin` p) -> do
        E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
        E.where_ (party E.^. PartyDay E.==. E.val today)
        let latDiff = p E.^. PlaceLat E.-. E.val (placeLat place)
        let lonDiff = p E.^. PlaceLon E.-. E.val (placeLon place)
        let latDiffSquared = latDiff E.*. latDiff
        let lonDiffSquared = lonDiff E.*. lonDiff
        -- Luckily the square function is monotone so we don't need to sqrt here
        let distSquared = latDiffSquared E.+. lonDiffSquared
        E.orderBy [E.asc distSquared]
        pure party

  withNavBar $(widgetFile "search")
