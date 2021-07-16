{-# LANGUAGE LambdaCase #-}

module Salsa.Party.Web.Server.Handler.I18NSpec (spec) where

import Data.List
import Data.Maybe
import Path
import Path.IO
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  describe "I18N" $
    it "Has translations for every string" $ do
      translationsDir <- resolveDir' "messages"
      mainFile <- resolveFile translationsDir "en.msg"
      allFiles <- snd <$> listDir translationsDir
      let otherFiles = filter (/= mainFile) allFiles
      mainFileContents <- readFile $ fromAbsFile mainFile
      let mainMessages = messagesIn mainFileContents
      tups <- forM otherFiles $ \otherFile -> do
        otherFileContents <- readFile $ fromAbsFile otherFile
        let missingMessages = mainMessages \\ messagesIn otherFileContents
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
