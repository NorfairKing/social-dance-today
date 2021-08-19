{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.I18NSpec (spec) where

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
import Salsa.Party.Web.Server.Handler.TestImport
import System.Exit

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

        forM_ mainMessages $ \mainMessage ->
          it (show mainMessage <> " is used") $ case M.lookup mainMessage usageMap of
            Just n | n > 0 -> pure ()
            _ -> expectationFailure $ unwords ["Message is unused:", show mainMessage]

        -- For each translation
        scenarioDir "messages" $ \fp -> do
          otherFileContents <- liftIO $ SB.readFile fp
          case TE.decodeUtf8' otherFileContents of
            Left _ -> pure () -- Probably a .swp file, let's just ignore it.
            Right otherContents -> do
              it (fp <> " has no more TODOs") $ do
                when ("TODO" `T.isInfixOf` otherContents) $ expectationFailure $ unwords [fp, "still contains TODOs"]
              describe (fp <> " has translations for every string") $ do
                let otherMessages = messagesIn (T.unpack otherContents)
                forM_ mainMessages $ \mainMessage ->
                  it ("Has a translation for " <> show mainMessage) $
                    if S.member mainMessage otherMessages
                      then pure ()
                      else
                        expectationFailure $
                          unwords
                            [ "Translation for",
                              show mainMessage,
                              "not found in",
                              fp
                            ]

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
