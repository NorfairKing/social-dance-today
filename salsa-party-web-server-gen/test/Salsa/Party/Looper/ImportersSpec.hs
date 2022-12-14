{-# OPTIONS_GHC -Wno-orphans #-}

module Salsa.Party.Looper.ImportersSpec (spec) where

import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import qualified Data.Map as M
import Salsa.Party.Looper.Importers
import Test.Syd
import Test.Syd.Validity

instance GenValid SoonestRun

spec :: Spec
spec = do
  describe "computeImporterToRun" $ do
    it "Chooses a given value" $
      forAllValid $ \interval ->
        forAllValid $ \now ->
          forAllValid $ \importers ->
            case computeImporterToRun interval now importers of
              Nothing -> pure () -- Fine
              Just i -> case M.lookup (i :: Int) importers of
                Nothing -> expectationFailure "Must choose a value from the list"
                _ -> pure () -- Fine
    it "Never chooses a DontRun value" $
      forAllValid $ \interval ->
        forAllValid $ \now ->
          forAllValid $ \importers ->
            case computeImporterToRun interval now importers of
              Nothing -> pure () -- Fine
              Just i -> case M.lookup (i :: Int) importers of
                Just (DontRun, _) -> expectationFailure "Must not choose a DontRun"
                _ -> pure () -- Fine
    it "Chooses an ASAP if one is available" $
      forAllValid $ \interval ->
        forAllValid $ \now ->
          forAllValid $ \importers ->
            forAllValid $ \v ->
              forAllValid $ \offset ->
                let ls = M.insert (v :: Int) (RunASAP, offset) importers
                 in case computeImporterToRun interval now ls of
                      Nothing -> expectationFailure "Should have chosen the ASAP, but chose none"
                      Just i -> case M.lookup i ls of
                        Just (RunASAP, _) -> pure ()
                        Just actual -> expectationFailure $ unwords ["Should have chosen the ASAP, but choose:", show actual]
                        Nothing -> expectationFailure "Should have chosen one of the values"
    it "Prefers a sooner 'NoSoonerThan' in the same timezone" $ do
      forAllValid $ \interval ->
        forAllValid $ \now ->
          forAllValid $ \(v1, v2) ->
            forAllValid $ \(t1, t2) ->
              forAllValid $ \offset ->
                let ls =
                      M.fromList
                        [ (v1, (RunNoSoonerThan (min t1 t2), offset)),
                          (v2, (RunNoSoonerThan (max t1 t2), offset))
                        ]
                 in case computeImporterToRun interval now ls of
                      Nothing -> pure () -- Fine, neither of them are in the future
                      Just i -> i `shouldBe` (v1 :: Int)
