{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.Scalpel.Extended where

import Conduit
import Control.Applicative
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

maybeUtf8 sb = case TE.decodeUtf8' (LB.toStrict sb) of
  Left _ -> Nothing
  Right t -> Just t

mutf8 :: Functor m => ScraperT LB.ByteString m (Maybe LB.ByteString) -> ScraperT LB.ByteString m (Maybe Text)
mutf8 = fmap (>>= maybeUtf8)

-- Only use this one when it's necessary.
utf8 :: LB.ByteString -> ScraperT LB.ByteString Import Text
utf8 lb = case maybeUtf8 lb of
  Nothing -> fail "Invalid UTF8"
  Just t -> pure t
