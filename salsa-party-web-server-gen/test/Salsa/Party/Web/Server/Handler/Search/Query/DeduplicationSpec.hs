{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Search.Query.DeduplicationSpec (spec) where

import Data.Aeson as JSON
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Database.Persist as DB
import Path
import Path.IO
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent

readExternalEventsMap :: Path Abs Dir -> IO (Map (Path Rel Dir) (Map (Path Rel File) ExternalEventExport))
readExternalEventsMap externalEventsDir = do
  externalEventUnitDirs <- fst <$> listDir externalEventsDir
  fmap M.fromList $
    forM externalEventUnitDirs $ \unitDir -> do
      files <- snd <$> listDir unitDir
      filesMap <- forM files $ \file -> do
        errOrExternalEvent <- JSON.eitherDecodeFileStrict' (fromAbsFile file)
        case errOrExternalEvent of
          Left err -> expectationFailure $ unlines ["Could not parse external events file:" <> fromAbsFile file, err]
          Right externalEvent -> pure (filename file, externalEvent)
      pure (dirname unitDir, M.fromList filesMap)

tuples :: [b] -> [(b, b)]
tuples = \case
  [] -> []
  (b : bs) -> map ((,) b) bs ++ tuples bs

importExternalEvent :: MonadIO m => ExternalEventExport -> SqlPersistT m (Entity ExternalEvent, Entity Place, Maybe CASKey)
importExternalEvent export = do
  externalEventEntity@(Entity externalEventId externalEvent) <- importExternalEventExport export
  mPlace <- DB.get (externalEventPlace externalEvent)
  placeEntity <- case mPlace of
    Nothing -> liftIO $ expectationFailure "Place not found."
    Just place -> pure (Entity (externalEventPlace externalEvent) place)
  mCasKey <- getPosterForExternalEvent externalEventId
  pure (externalEventEntity, placeEntity, mCasKey)

spec :: Spec
spec = do
  dbSpec $
    describe "externalEventIsSimilarEnoughToParty" $ do
      externalEvents <- liftIO $ do
        deduplicationDir <- resolveDir' "test_resources/deduplication"
        -- partyDir <- resolveDir deduplicationDir "party"
        externalEventDir <- resolveDir deduplicationDir "external-event"
        readExternalEventsMap externalEventDir

      describe "positives" $
        forM_ (M.toList externalEvents) $ \(eventName, externalEventExports) ->
          describe (fromRelDir eventName) $
            forM_ (tuples (M.toList externalEventExports)) $ \((exportFile1, externalEventExport1), (exportFile2, externalEventExport2)) -> do
              let itDescription =
                    unwords
                      [ "says that",
                        fromRelFile exportFile1,
                        "and",
                        fromRelFile exportFile2,
                        "are duplicates"
                      ]
              it itDescription $ \pool -> runPersistentTest pool $ do
                tup1 <- importExternalEvent externalEventExport1
                tup2 <- importExternalEvent externalEventExport2
                let similar = externalEventIsSimilarEnoughTo tup1 tup2
                if similar
                  then pure ()
                  else
                    liftIO $
                      expectationFailure $
                        unlines
                          [ "These external events were not considered duplicates but they should have been:",
                            ppShow externalEventExport1,
                            ppShow externalEventExport2
                          ]

      describe "negatives" $
        forM_ (tuples (M.toList externalEvents)) $ \((exportDir1, filesMap1), (exportDir2, filesMap2)) -> do
          let description1 = unwords [fromRelDir exportDir1, "and", fromRelDir exportDir2]
          describe description1 $
            forM_ ((,) <$> M.toList filesMap1 <*> M.toList filesMap2) $ \((exportFile1, externalEventExport1), (exportFile2, externalEventExport2)) -> do
              let itDescription =
                    unwords
                      [ "says that",
                        fromRelFile exportFile1,
                        "and",
                        fromRelFile exportFile2,
                        "are duplicates"
                      ]
              it itDescription $ \pool -> runPersistentTest pool $ do
                tup1 <- importExternalEvent externalEventExport1
                tup2 <- importExternalEvent externalEventExport2
                let similar = externalEventIsSimilarEnoughTo tup1 tup2
                if similar
                  then
                    liftIO $
                      expectationFailure $
                        unlines
                          [ "These external events were considered duplicates but they should not have been:",
                            ppShow externalEventExport1,
                            ppShow externalEventExport2
                          ]
                  else pure ()

  describe "descriptionCloseEnoughTo" $ do
    it "considers these salsavida descriptions equal." $ do
      t1 <- T.readFile "test_resources/deduplication/description/1a.txt"
      t2 <- T.readFile "test_resources/deduplication/description/1b.txt"
      unless (descriptionCloseEnoughTo (Just t1) (Just t2)) $ expectationFailure "Should have been considered close enough."
    it "does not consider two Nothings equal" $
      not $ descriptionCloseEnoughTo Nothing Nothing
    it "does not consider two empty descriptions equal" $
      not $ descriptionCloseEnoughTo (Just "") (Just "")
  describe "placeCloseEnough" $ do
    it "Comma in address" $
      placeCloseEnoughTo
        "Viaduktstrasse 67, 8005 Zürich"
        "Viaduktstrasse 67 8005 Zürich"
  describe "titleCloseEnoughTo" $ do
    it "Zouk party" $
      titleCloseEnoughTo
        "Zouk meets Bachata @Bürkliplatz 😊🎵"
        "ZOUK meets BACHATA Party @Bürkliplatz 😊🎵"
    it "Extra letter somewhere" $
      titleCloseEnoughTo
        "Bachata Community Zürich Monday 💃🕺"
        "Bachata Community Zürich Mondays 💃🕺"
    it "Different casing" $
      titleCloseEnoughTo
        "Noche Latina mit Powell und DJ Ñoño"
        "NOCHE LATINA - mit Powell und DJ Ñoño"
    it "Extra nonsense" $
      titleCloseEnoughTo
        "Bachateros Treff"
        "BACHATEROS TREFF ★★★★★"
    it "Completely different" $
      not $
        titleCloseEnoughTo
          "Bachata Community Zürich Monday 💃🕺"
          "Lounge@Bananenreiferei"
    it "Completely different" $
      not $
        titleCloseEnoughTo
          "Social Salsa Party"
          "Free workshops!"
    it "Close but different" $
      not $
        titleCloseEnoughTo
          "Syd's birthday party"
          "Josh's birthday party"
    it "Completely different but all symbols" $
      not $
        titleCloseEnoughTo
          "平仮名"
          "漢字"
