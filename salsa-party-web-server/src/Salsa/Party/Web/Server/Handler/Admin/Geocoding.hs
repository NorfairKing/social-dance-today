{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Geocoding
  ( getAdminGeocodingR,
    postAdminGeocodingR,
  )
where

import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getAdminGeocodingR :: Handler Html
getAdminGeocodingR = adminGeocodingPage Nothing

data Geocoding = Geocoding {geocodingQuery :: Text}

geocodingForm :: FormInput Handler Geocoding
geocodingForm = Geocoding <$> ireq textField "query"

postAdminGeocodingR :: Handler Html
postAdminGeocodingR = do
  result <- runInputPostResult geocodingForm
  adminGeocodingPage $ Just result

adminGeocodingPage :: Maybe (FormResult Geocoding) -> Handler Html
adminGeocodingPage mResult = do
  mPlace <- case mResult of
    Just (FormSuccess Geocoding {..}) -> Just <$> lookupPlace geocodingQuery
    _ -> pure Nothing
  let mkLocation place = Location {locationPlace = place}
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "admin/geocoding")
