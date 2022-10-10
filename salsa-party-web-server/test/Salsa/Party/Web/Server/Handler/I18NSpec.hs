{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- This module makes sure that we didn't forget to translate anything, or translated too much.
-- It has to be in this package because it deals with the messages dir which is in this library.
module Salsa.Party.Web.Server.Handler.I18NSpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import System.Exit
import Test.Syd

spec :: Spec
spec =
  describe "I18N" $ do
    translationsDir <- resolveDir' "messages"
    mainFile <- resolveFile translationsDir "en.msg"
    mainFileContents <- liftIO $ SB.readFile $ fromAbsFile mainFile
    case TE.decodeUtf8' mainFileContents of
      Left _ -> liftIO $ die "main file did not contain valid utf8"
      Right mainContents -> do
        let mainMessages = messagesIn (T.unpack mainContents)

        let dirs :: [FilePath]
            dirs = ["src", "templates"]
        usageMap <- liftIO $ do
          files <- fmap concat $
            forM dirs $ \dir -> do
              adir <- resolveDir' dir
              snd <$> listDirRecur adir
          fmap (M.unionsWith (+)) $
            forM files $ \file -> do
              contents <- SB.readFile $ fromAbsFile file
              pure $ usageMapFor mainMessages contents

        specify "All messages are used" $ do
          let unusedMessages = filter ((== 0) . fromMaybe 0 . (`M.lookup` usageMap)) (S.toList mainMessages)
          when (not (null unusedMessages)) $
            expectationFailure $ unlines ("Unused messages: " : map show unusedMessages)

        -- For each translation
        scenarioDir "messages" $ \fp -> do
          otherFileContents <- liftIO $ SB.readFile fp
          case TE.decodeUtf8' otherFileContents of
            Left _ -> pure () -- Probably a .swp file, let's just ignore it.
            Right otherContents -> do
              specify (fp <> " has no more TODOs") $ do
                let otherLines = T.lines otherContents
                    linesWithTODOs = catMaybes $ pythonLikeIterate (\ix line -> if "TODO" `T.isInfixOf` line then Just (ix, line) else Nothing) otherLines
                when (not (null linesWithTODOs)) $
                  expectationFailure $
                    unlines
                      ( "Lines with TODO:" :
                        map
                          ( \(ix, line) ->
                              unwords
                                [ show ix <> ":",
                                  T.unpack line
                                ]
                          )
                          linesWithTODOs
                      )

              specify (fp <> " has translations for every string") $ do
                let otherMessages = messagesIn (T.unpack otherContents)
                let untranslatedMessages = filter (not . (`S.member` otherMessages)) (S.toList mainMessages)
                when (not (null untranslatedMessages)) $
                  expectationFailure $ unlines ("Untranslated messages:" : map show untranslatedMessages)

pythonLikeIterate :: (Int -> a -> b) -> [a] -> [b]
pythonLikeIterate func = zipWith func [0 ..]

usageMapFor :: Set Text -> ByteString -> Map Text Word
usageMapFor messages contents = case TE.decodeUtf8' contents of
  Left _ -> M.empty
  Right textContents -> flip M.fromSet messages $ \message ->
    if ("Msg" <> message) `T.isInfixOf` textContents
      then 1
      else 0

messagesIn :: String -> Set Text
messagesIn contents =
  let firstWords :: [String]
      firstWords = mapMaybe (listToMaybe . words) $ lines contents
      stripColumn = \case
        ':' : s -> s
        s -> s
      dropColumn = reverse . stripColumn . reverse
   in S.fromList $ map (T.pack . dropColumn) $ filter (not . all (== '#')) firstWords
