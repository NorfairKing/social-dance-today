{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Gen where

import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Party

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid = fmap Textarea . shrinkValid . unTextarea

instance GenValid Location where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Place where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Party where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
