{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.URI where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Network.URI

instance FromJSON URI where
  parseJSON = withText "URI" $ \t -> case parseURI (T.unpack t) of
    Nothing -> fail $ "Invalid URI: " <> T.unpack t
    Just u -> pure u

instance PersistFieldSql URI where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

instance PersistField URI where
  toPersistValue = toPersistValue . show
  fromPersistValue pv = do
    s <- fromPersistValue pv
    case parseURI s of
      Nothing -> Left "Invalid URI"
      Just uri -> pure uri
