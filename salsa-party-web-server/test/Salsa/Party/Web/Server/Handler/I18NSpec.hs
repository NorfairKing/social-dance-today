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
    scenarioDir "messages" $ \fp ->
      it (fp <> " has no more TODOs") $ do
        contents <- SB.readFile fp
        contents `shouldNotSatisfy` ("TODO" `SB.isInfixOf`)

    it "Has translations for every string" $ do
      translationsDir <- resolveDir' "messages"
      mainFile <- resolveFile translationsDir "en.msg"
      allFiles <- snd <$> listDir translationsDir
      let otherFiles = filter (/= mainFile) allFiles
      mainFileContents <- SB.readFile $ fromAbsFile mainFile
      case TE.decodeUtf8' mainFileContents of
        Left _ -> expectationFailure "main file did not contain valid utf8"
        Right mainContents -> do
          let mainMessages = messagesIn (T.unpack mainContents)
          tups <- forM otherFiles $ \otherFile -> do
            otherFileContents <- SB.readFile $ fromAbsFile otherFile
            case TE.decodeUtf8' otherFileContents of
              Left _ -> pure (filename otherFile, [])
              Right otherContents -> do
                let missingMessages = mainMessages \\ messagesIn (T.unpack otherContents)
                pure (filename otherFile, missingMessages)
          when (not (all (null . snd) tups)) $
            expectationFailure $
              unlines $
                flip map tups $ \(name, missingMessages) ->
                  unlines $
                    unwords ["Missing messages in", fromRelFile name] : missingMessages

messagesIn :: String -> [String]
messagesIn contents =
  let firstWords :: [String]
      firstWords = mapMaybe (listToMaybe . words) $ lines contents
      stripColumn = \case
        ':' : s -> s
        s -> s
      dropColumn = reverse . stripColumn . reverse
   in map dropColumn $ filter (/= "#") firstWords
