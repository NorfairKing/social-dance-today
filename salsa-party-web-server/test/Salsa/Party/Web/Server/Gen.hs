{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Gen where

import Control.Monad
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Import
import Test.QuickCheck

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid = fmap Textarea . shrinkValid . unTextarea

instance GenValid CASKey where
  shrinkValid _ = [] -- No point, it's a hash.
  genValid = mkCASKey <$> genValid <*> genValid

instance GenValid OrganiserForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Coordinates where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddPartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditPartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Place where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Organiser where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Party where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEvent where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PartyPoster where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Image where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- This isn't very general, but that's probably fine.
genValidEmailAddress :: Gen Text
genValidEmailAddress = do
  userLen <- upTo 10
  user <- replicateM (max 1 userLen) $ choose ('a', 'z')
  domainLen <- upTo 10
  domain <- replicateM (max 1 domainLen) $ choose ('a', 'z')
  tldLen <- upTo 10
  tld <- replicateM (max 1 tldLen) $ choose ('a', 'z')
  pure $ T.pack $ concat [user, "@", domain, ".", tld]

genValidPassword :: Gen Text
genValidPassword = genValid `suchThat` (not . T.null)
