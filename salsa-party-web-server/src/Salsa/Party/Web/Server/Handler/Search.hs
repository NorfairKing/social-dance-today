{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import OpenStreetMaps.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getSearchR :: Text -> Handler Html
getSearchR placeQuery = do
  man <- getsYesod appHTTPManager
  let req = GeocodingRequest {geocodingRequestQuery = placeQuery}
  resp <- liftIO $ makeGeocodingRequest man req
  withNavBar $(widgetFile "search")
