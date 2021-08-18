{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.I18NSpec (spec) where

import qualified Data.ByteString as SB
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
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
        scenarioDir "messages" $ \fp -> do
          otherFileContents <- liftIO $ SB.readFile fp
          case TE.decodeUtf8' otherFileContents of
            Left _ -> pure () -- Probably a .swp file, let's just ignore it.
            Right otherContents -> do
              it (fp <> " has no more TODOs") $ do
                otherContents `shouldNotSatisfy` ("TODO" `T.isInfixOf`)
              describe (fp <> " has translations for every string") $ do
                let otherMessages = messagesIn (T.unpack otherContents)
                forM_ mainMessages $ \mainMessage ->
                  it ("Has a translation for " <> mainMessage) $
                    if S.member mainMessage otherMessages
                      then pure ()
                      else
                        expectationFailure $
                          unwords
                            [ "Translation for",
                              mainMessage,
                              "not found in",
                              fp
                            ]

messagesIn :: String -> Set String
messagesIn contents =
  let firstWords :: [String]
      firstWords = mapMaybe (listToMaybe . words) $ lines contents
      stripColumn = \case
        ':' : s -> s
        s -> s
      dropColumn = reverse . stripColumn . reverse
   in S.fromList $ map dropColumn $ filter (not . all (== '#')) firstWords
