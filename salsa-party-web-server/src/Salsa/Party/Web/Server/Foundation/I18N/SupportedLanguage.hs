{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-fields #-}

module Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import Data.Validity.Text ()
import Data.Validity.Time ()
import Yesod

-- I18N Languages
data SupportedLanguage
  = SupportedLangEnglish
  | SupportedLangGerman
  | SupportedLangDutch
  deriving (Show, Read, Eq, Enum, Bounded)

instance PathPiece SupportedLanguage where
  fromPathPiece = parseSupportedLanguage
  toPathPiece = supportedLanguageAbbreviation

supportedLanguages :: [SupportedLanguage]
supportedLanguages = [minBound .. maxBound]

parseSupportedLanguage :: Text -> Maybe SupportedLanguage
parseSupportedLanguage =
  \case
    "en" -> Just SupportedLangEnglish
    "de" -> Just SupportedLangGerman
    "nl" -> Just SupportedLangDutch
    _ -> Nothing

supportedLanguageAbbreviation :: SupportedLanguage -> Text
supportedLanguageAbbreviation =
  \case
    SupportedLangEnglish -> "en"
    SupportedLangGerman -> "de"
    SupportedLangDutch -> "nl"

supportedLanguageNative :: SupportedLanguage -> Text
supportedLanguageNative =
  \case
    SupportedLangEnglish -> "English"
    SupportedLangGerman -> "Deutsch"
    SupportedLangDutch -> "Nederlands"

supportedLanguageEnglish :: SupportedLanguage -> Text
supportedLanguageEnglish =
  \case
    SupportedLangEnglish -> "English"
    SupportedLangGerman -> "German"
    SupportedLangDutch -> "Dutch"

getFirstMatchingSupportedLanguage :: MonadHandler m => m SupportedLanguage
getFirstMatchingSupportedLanguage = do
  ls <- languages
  pure $ fromMaybe SupportedLangEnglish $ firstMatchingSupportedLanguage ls

firstMatchingSupportedLanguage :: [Text] -> Maybe SupportedLanguage
firstMatchingSupportedLanguage =
  \case
    [] -> Nothing
    (l : ls) -> parseSupportedLanguage l <|> firstMatchingSupportedLanguage ls
