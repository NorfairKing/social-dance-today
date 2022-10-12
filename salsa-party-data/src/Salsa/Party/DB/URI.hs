{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.URI where

import Data.Aeson
import qualified Data.Text as T
import Network.URI

instance FromJSON URI where
  parseJSON = withText "URI" $ \t -> case parseURI (T.unpack t) of
    Nothing -> fail $ "Invalid URI: " <> T.unpack t
    Just u -> pure u
