{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Search.DeduplicationSpec (spec) where

import Data.Aeson as JSON
import qualified Data.ByteString as SB
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Path
import Path.IO
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Export
import Salsa.Party.Web.Server.Handler.Event.Party.Export
import Salsa.Party.Web.Server.Handler.Search.Deduplication
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent

spec :: Spec
spec = do
  describe "Similarities" $ do
    genValidSpec @Similarity
    genValidSpec @SimilarityFormula
    describe "computeSimilarityFormula" $
      it "produces valid similarities" $ producesValid computeSimilarityFormula
    describe "sumSimilarities" $ do
      it "says 0 for this formula" $ sumSimilarities [(0.0, Similarity 1)] `shouldBe` Similarity 0
    describe "placeSimilarity" $ similarityFormulaFunctionSpec placeSimilarity
    describe "textSimilarity" $ similarityFunctionSpec textSimilarity
    describe "timeSimilarity" $ similarityFunctionSpec timeSimilarity
    describe "rationalSimilarity" $ similarityFunctionSpec rationalSimilarity
    describe "boolSimilarity" $ similarityFunctionSpec rationalSimilarity

  dbSpec $
    describe "externalEventIsSimilarEnoughToParty" $ do
      deduplicationDir <- liftIO $ resolveDir' "test_resources/deduplication"
      eventsDir <- liftIO $ resolveDir deduplicationDir "event"
      eventExports <- liftIO $ readEventsMap eventsDir

      it "recognises positives correctly" $ \pool ->
        forM_ (M.toList eventExports) $ \(dayDir, eventExportsPerDay) ->
          forM_ (M.toList eventExportsPerDay) $ \(eventName, externalEventExports) ->
            forM_ (tuples (M.toList externalEventExports)) $ \(t1, t2) ->
              runPersistentTest pool $
                positiveTest (eventsDir </> dayDir </> eventName) t1 t2

      it "recognises negatives correctly" $ \pool ->
        forM_ (M.toList eventExports) $ \(dayDir, eventExportsPerDay) ->
          forM_ (tuples (M.toList eventExportsPerDay)) $ \((exportDir1, filesMap1), (exportDir2, filesMap2)) -> do
            forM_ ((,) <$> M.toList filesMap1 <*> M.toList filesMap2) $ \(t1, t2) ->
              runPersistentTest pool $
                negativeTest (eventsDir </> dayDir </> exportDir1) (eventsDir </> dayDir </> exportDir2) t1 t2

data AnyExport
  = InternalExport PartyExport
  | ExternalExport ExternalEventExport
  deriving (Show, Eq)

readEventsMap :: Path Abs Dir -> IO (Map (Path Rel Dir) (Map (Path Rel Dir) (Map (Path Rel File) AnyExport)))
readEventsMap externalEventsDir = do
  dayDirs <- fst <$> listDir externalEventsDir
  fmap M.fromList $
    forM dayDirs $ \dayDir -> do
      externalEventUnitDirs <- fst <$> listDir dayDir
      eventsPerDay <- fmap M.fromList $
        forM externalEventUnitDirs $ \unitDir -> do
          files <- snd <$> listDir unitDir
          filesMap <- forM files $ \file -> do
            contents <- SB.readFile (fromAbsFile file)
            case JSON.eitherDecodeStrict' contents of
              Right externalEvent -> pure (filename file, ExternalExport externalEvent)
              Left err1 -> case JSON.eitherDecodeStrict' contents of
                Right party -> pure (filename file, InternalExport party)
                Left err2 -> expectationFailure $ unlines ["Could not parse event file:" <> fromAbsFile file, err1, err2]
          pure (dirname unitDir, M.fromList filesMap)
      pure (dirname dayDir, eventsPerDay)

tuples :: [b] -> [(b, b)]
tuples = \case
  [] -> []
  (b : bs) -> map ((,) b) bs ++ tuples bs

importParty :: MonadIO m => PartyExport -> SqlPersistT m (Entity Organiser, Entity Party, Entity Place)
importParty export = do
  partyEntity@(Entity _ party) <- importPartyExport export
  mOrganiser <- DB.get (partyOrganiser party)
  organiserEntity <- case mOrganiser of
    Nothing -> liftIO $ expectationFailure "Organiser not found."
    Just organiser -> pure (Entity (partyOrganiser party) organiser)
  mPlace <- DB.get (partyPlace party)
  placeEntity <- case mPlace of
    Nothing -> liftIO $ expectationFailure "Place not found."
    Just place -> pure (Entity (partyPlace party) place)
  pure (organiserEntity, partyEntity, placeEntity)

importExternalEvent :: MonadIO m => ExternalEventExport -> SqlPersistT m (Entity ExternalEvent, Entity Place)
importExternalEvent export = do
  externalEventEntity@(Entity _ externalEvent) <- importExternalEventExport export
  mPlace <- DB.get (externalEventPlace externalEvent)
  placeEntity <- case mPlace of
    Nothing -> liftIO $ expectationFailure "Place not found."
    Just place -> pure (Entity (externalEventPlace externalEvent) place)
  pure (externalEventEntity, placeEntity)

positiveTest :: Path Abs Dir -> (Path Rel File, AnyExport) -> (Path Rel File, AnyExport) -> DB.SqlPersistM ()
positiveTest rootDir t1@(exportFile1, export1) t2@(exportFile2, export2) =
  case (export1, export2) of
    (InternalExport _, ExternalExport _) -> positiveTest rootDir t2 t1 -- Assuming that duplicateness is symmetric
    (InternalExport _, InternalExport _) -> pure () -- Nothing to test yet.
    (ExternalExport externalEventExport1, InternalExport partyExport2) -> do
      tup1 <- importExternalEvent externalEventExport1
      tup2 <- importParty partyExport2
      let similar = externalEventIsSimilarEnoughToParty tup1 tup2
      if similar
        then pure ()
        else
          liftIO $
            expectationFailure $
              unlines
                [ "This external event was not considered a duplicate of this party but it should have been:",
                  fromAbsFile (rootDir </> exportFile1),
                  ppShow externalEventExport1,
                  fromAbsFile (rootDir </> exportFile2),
                  ppShow partyExport2,
                  ppShow $ similarityScoreExternalToInternal tup1 tup2,
                  ppShow $ computeSimilarityFormula $ similarityScoreExternalToInternal tup1 tup2
                ]
    (ExternalExport externalEventExport1, ExternalExport externalEventExport2) -> do
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
                  fromAbsFile (rootDir </> exportFile1),
                  ppShow externalEventExport1,
                  fromAbsFile (rootDir </> exportFile2),
                  ppShow externalEventExport2,
                  ppShow $ similarityScoreExternalToExternal tup1 tup2,
                  ppShow $ computeSimilarityFormula $ similarityScoreExternalToExternal tup1 tup2
                ]

negativeTest :: Path Abs Dir -> Path Abs Dir -> (Path Rel File, AnyExport) -> (Path Rel File, AnyExport) -> DB.SqlPersistM ()
negativeTest rootDir1 rootDir2 t1@(exportFile1, export1) t2@(exportFile2, export2) = do
  case (export1, export2) of
    (InternalExport _, ExternalExport _) -> negativeTest rootDir2 rootDir1 t2 t1 -- Assuming that duplicateness is symmetric
    (InternalExport _, InternalExport _) -> pure () -- Nothing to test yet.
    (ExternalExport externalEventExport1, InternalExport partyExport2) -> do
      tup1 <- importExternalEvent externalEventExport1
      tup2 <- importParty partyExport2
      let similar = externalEventIsSimilarEnoughToParty tup1 tup2
      if similar
        then
          liftIO $
            expectationFailure $
              unlines
                [ "These external events were considered duplicates but they should not have been:",
                  fromAbsFile (rootDir1 </> exportFile1),
                  ppShow externalEventExport1,
                  fromAbsFile (rootDir2 </> exportFile2),
                  ppShow partyExport2,
                  ppShow $ similarityScoreExternalToInternal tup1 tup2,
                  ppShow $ computeSimilarityFormula $ similarityScoreExternalToInternal tup1 tup2
                ]
        else pure ()
    (ExternalExport externalEventExport1, ExternalExport externalEventExport2) -> do
      tup1 <- importExternalEvent externalEventExport1
      tup2 <- importExternalEvent externalEventExport2
      let similar = externalEventIsSimilarEnoughTo tup1 tup2
      if similar
        then
          liftIO $
            expectationFailure $
              unlines
                [ "These external events were considered duplicates but they should not have been:",
                  fromAbsFile (rootDir1 </> exportFile1),
                  ppShow externalEventExport1,
                  fromAbsFile (rootDir2 </> exportFile2),
                  ppShow externalEventExport2,
                  ppShow $ similarityScoreExternalToExternal tup1 tup2,
                  ppShow $ computeSimilarityFormula $ similarityScoreExternalToExternal tup1 tup2
                ]
        else pure ()

similarityFormulaFunctionSpec :: (Show a, GenValid a) => (a -> a -> SimilarityFormula) -> Spec
similarityFormulaFunctionSpec func = similarityFunctionSpec (\a1 a2 -> computeSimilarityFormula (func a1 a2))

similarityFunctionSpec :: (Show a, GenValid a) => (a -> a -> Similarity) -> Spec
similarityFunctionSpec similar = do
  it "produces valid similarities" $
    forAllValid $ \a1 ->
      forAllValid $ \a2 ->
        shouldBeValid $ similar a1 a2
  it "scores equal things as 1" $
    forAllValid $ \a -> similar a a `shouldBeCloseEnough` Similarity 1
  it "is symmetric" $
    forAllValid $ \a ->
      forAllValid $ \b ->
        similar a b `shouldBeCloseEnough` similar b a

shouldBeCloseEnough :: Similarity -> Similarity -> IO ()
shouldBeCloseEnough (Similarity d1) (Similarity d2) = abs (d1 - d2) `shouldSatisfy` (< 1E-16)
