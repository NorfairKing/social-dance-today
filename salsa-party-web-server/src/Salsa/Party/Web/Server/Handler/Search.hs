{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Search where

import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getQueryR :: Handler Html
getQueryR = do
  placeQuery <- runInputGet $ ireq textField "query"
  redirect $ SearchR placeQuery

getSearchR :: Text -> Handler Html
getSearchR placeQuery = do
  Entity _ place <- lookupPlace placeQuery
  withNavBar $(widgetFile "search")
