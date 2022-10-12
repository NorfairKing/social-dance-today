{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Salsa.Party.DB.Gen ()
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Export
import Salsa.Party.Web.Server.Handler.Event.JSON.Place
import Salsa.Party.Web.Server.Handler.Event.Party.Export
import Salsa.Party.Web.Server.Handler.Search.Deduplication
import Test.QuickCheck

instance GenValid UserExport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OrganiserForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddPartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditPartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PlaceExport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OrganiserExport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PartyExport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExternalEventExport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AddScheduleForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditScheduleForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Similarity where
  genValid = Similarity <$> choose (0, 1)
  shrinkValid = shrinkValidStructurally

instance GenValid SimilarityFormula where
  shrinkValid = shrinkValidStructurally
  genValid = (`suchThat` isValid) $
    sized $ \case
      0 -> Factor <$> genValid <*> genValid
      _ ->
        oneof
          [ Factor <$> genValid <*> genValid,
            Terms <$> genValid <*> genValid
          ]
