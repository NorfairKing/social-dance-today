{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.DB.Slug where

import Control.Monad
import Data.Aeson as JSON
import qualified Data.Char as Char
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.ICU as ICU
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as HTTP
import Text.Read
import Web.HttpApiData
import Web.PathPieces

newtype Slug a = Slug {unSlug :: Text}
  deriving (Eq, Ord, Generic)

instance Validity (Slug a) where
  validate s@Slug {..} =
    mconcat
      [ genericValidate s,
        declare "it is not empty" $
          not $ T.null unSlug,
        decorateList (T.unpack unSlug) validateSlugChar,
        declare "it doesn't have consequtive replacement chars" $
          not $ T.pack [replacementChar, replacementChar] `T.isInfixOf` unSlug,
        declare "it doesn't have replacement characters at the start" $
          not $ T.singleton replacementChar `T.isPrefixOf` unSlug,
        declare "it doesn't have replacement characters at the end" $
          not $ T.singleton replacementChar `T.isSuffixOf` unSlug,
        declare "it doesn't need url encoding" $
          let encoded = TE.encodeUtf8 unSlug
           in HTTP.urlEncode False encoded == encoded
      ]

validateSlugChar :: Char -> Validation
validateSlugChar c
  | c == replacementChar = valid
  | otherwise =
      mconcat
        [ declare "The character is printable" $ Char.isPrint c,
          declare "The character is not upper-case" $ not $ Char.isUpper c,
          declare "The character is not a space character" $ not $ Char.isSpace c,
          declare "The character is alphanumeric" $ Char.isAlphaNum c,
          declare "The character is in Latin1" $ Char.isAscii c
        ]

instance Show (Slug a) where
  show = T.unpack . unSlug

instance Read (Slug a) where
  readPrec = do
    t <- readPrec
    case mkSlug t of
      Nothing -> fail "Couldn't make a slug"
      Just slug -> pure slug

instance PathPiece (Slug a) where
  toPathPiece = unSlug
  fromPathPiece = fmap Slug . fromPathPiece

instance FromHttpApiData (Slug a) where
  parseUrlPiece = fmap Slug . parseUrlPiece

instance ToHttpApiData (Slug a) where
  toUrlPiece = unSlug

instance PersistField (Slug a) where
  toPersistValue = toPersistValue . unSlug
  fromPersistValue = fmap Slug . fromPersistValue

instance PersistFieldSql (Slug a) where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

instance ToJSON (Slug a) where
  toJSON = toJSON . unSlug

instance FromJSON (Slug a) where
  parseJSON v = Slug <$> parseJSON v

mkSlug :: Text -> Maybe (Slug a)
mkSlug contents = do
  let unSlug =
        stripReplacements
          . T.pack
          . deduplicateReplacements
          . mapMaybe mkSlugChar
          . T.unpack
          . ICU.toCaseFold False
          . ICU.normalize ICU.NFD
          $ contents
  guard $ not $ T.null unSlug
  pure Slug {..}

deduplicateReplacements :: String -> String
deduplicateReplacements = go
  where
    go [] = []
    go [c] = [c]
    go (c1 : c2 : cs)
      | c1 == replacementChar && c2 == replacementChar = go (c2 : cs)
      | otherwise = c1 : go (c2 : cs)

stripReplacements :: Text -> Text
stripReplacements = T.dropWhile (== replacementChar) . T.dropWhileEnd (== replacementChar)

mkSlugChar :: Char -> Maybe Char
mkSlugChar c
  | Char.isSpace c = Just replacementChar
  | otherwise =
      if validationIsValid (validateSlugChar c)
        then Just c
        else Nothing

replacementChar :: Char
replacementChar = '-'
