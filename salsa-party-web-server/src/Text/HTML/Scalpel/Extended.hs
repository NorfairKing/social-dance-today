{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.Scalpel.Extended where

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Text.HTML.Scalpel

maybeUtf8 :: LB.ByteString -> Maybe Text
maybeUtf8 sb = case TE.decodeUtf8' (LB.toStrict sb) of
  Left _ -> Nothing
  Right t -> Just t

mutf8 :: Functor m => ScraperT LB.ByteString m (Maybe LB.ByteString) -> ScraperT LB.ByteString m (Maybe Text)
mutf8 = fmap (>>= maybeUtf8)

-- Only use this one when it's necessary.
utf8 :: Monad m => LB.ByteString -> ScraperT LB.ByteString m Text
utf8 lb = case maybeUtf8 lb of
  Nothing -> fail "Invalid UTF8"
  Just t -> pure t
