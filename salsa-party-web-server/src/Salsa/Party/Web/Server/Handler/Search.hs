{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import OpenStreetMaps.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getQueryR :: Handler Html
getQueryR = do
  placeQuery <- runInputGet $ ireq textField "query"
  redirect $ SearchR placeQuery

getSearchR :: Text -> Handler Html
getSearchR = searchPageFor

searchPageFor :: Text -> Handler Html
searchPageFor placeQuery = do
  man <- getsYesod appHTTPManager
  let req = GeocodingRequest {geocodingRequestQuery = placeQuery}
  resp <- liftIO $ makeGeocodingRequest man req
  withNavBar $(widgetFile "search")
