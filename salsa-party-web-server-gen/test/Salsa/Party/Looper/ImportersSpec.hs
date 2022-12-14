{-# OPTIONS_GHC -Wno-orphans #-}

module Salsa.Party.Looper.ImportersSpec (spec) where

import Data.GenValidity.Time ()
import Salsa.Party.Looper.Importers
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid SoonestRun

spec :: Spec
spec = do
  describe "computeImporterToRun" $ do
    it "Chooses a given value" $
      forAllValid $ \now ->
        forAllValid $ \importers ->
          case computeImporterToRun now importers of
            Nothing -> pure () -- Fine
            Just i -> case lookup (i :: Int) importers of
              Nothing -> expectationFailure "Must choose a value from the list"
              _ -> pure () -- Fine
    it "Never chooses a DontRun value" $
      forAllValid $ \now ->
        forAllValid $ \importers ->
          case computeImporterToRun now importers of
            Nothing -> pure () -- Fine
            Just i -> case lookup (i :: Int) importers of
              Just DontRun -> expectationFailure "Must not choose a DontRun"
              _ -> pure () -- Fine
    it "Chooses an ASAP if one is available" $
      forAllValid $ \now ->
        forAllValid $ \importers ->
          forAllValid $ \v ->
            forAll (shuffle ((v :: Int, RunASAP) : importers)) $ \ls ->
              case computeImporterToRun now ls of
                Nothing -> expectationFailure "Should have chosen the ASAP, but chose none"
                Just i -> case lookup i ls of
                  Just RunASAP -> pure ()
                  _ -> expectationFailure "Should have chosen the ASAP"
    it "Prefers a sooner 'NoSoonerThan'" $ do
      forAllValid $ \now ->
        forAllValid $ \(v1, v2) ->
          forAllValid $ \(t1, t2) ->
            forAll (shuffle [(v1, RunNoSoonerThan (min t1 t2)), (v2, RunNoSoonerThan (max t1 t2))]) $ \ls ->
              case computeImporterToRun now ls of
                Nothing -> pure () -- Fine, neither of them are in the future
                Just i -> i `shouldBe` (v1 :: Int)
