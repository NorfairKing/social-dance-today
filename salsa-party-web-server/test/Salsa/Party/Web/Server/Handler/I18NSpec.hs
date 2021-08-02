{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.I18NSpec (spec) where

import qualified Data.ByteString as SB
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  describe "I18N" $ do
    scenarioDir "messages" $ \fp -> do
      it (fp <> " has no more TODOs") $ do
        contents <- SB.readFile fp
        contents `shouldNotSatisfy` ("TODO" `SB.isInfixOf`)
      it (fp <> "has translations for every string") $ do
        translationsDir <- resolveDir' "messages"
        mainFile <- resolveFile translationsDir "en.msg"
        mainFileContents <- SB.readFile $ fromAbsFile mainFile
        case TE.decodeUtf8' mainFileContents of
          Left _ -> expectationFailure "main file did not contain valid utf8"
          Right mainContents -> do
            let mainMessages = messagesIn (T.unpack mainContents)
            otherFileContents <- SB.readFile fp
            case TE.decodeUtf8' otherFileContents of
              Left _ -> pure () -- Probably a .swp file, let's just ignore it.
              Right otherContents -> do
                let missingMessages = mainMessages \\ messagesIn (T.unpack otherContents)
                when (not (null missingMessages)) $
                  expectationFailure $
                    unlines $
                      unwords ["Missing messages in", fp] : missingMessages

messagesIn :: String -> [String]
messagesIn contents =
  let firstWords :: [String]
      firstWords = mapMaybe (listToMaybe . words) $ lines contents
      stripColumn = \case
        ':' : s -> s
        s -> s
      dropColumn = reverse . stripColumn . reverse
   in map dropColumn $ filter (not . all (== '#')) firstWords
