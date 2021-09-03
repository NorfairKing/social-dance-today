{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Gen where

import Control.Monad
import Data.Fixed
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Data.Password.Bcrypt
import qualified Data.Text as T
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Import
import Test.QuickCheck

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- No point.

instance GenValid (PasswordHash Bcrypt) where
  -- This is technically more than necessary, but we can't do any better because hashPassword runs in IO.
  genValid = PasswordHash <$> genValid
  shrinkValid _ = [] -- No point.

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid = fmap Textarea . shrinkValid . unTextarea

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid CASKey where
  shrinkValid _ = [] -- No point, it's a hash.
  genValid = mkCASKey <$> genValid <*> genValid

instance GenValid OrganiserForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Latitude where
  genValid = choose (-90_000_000_000, 90_000_000_000) `suchThatMap` (mkLatitude . MkFixed)
  shrinkValid = shrinkValidStructurally

instance GenValid Longitude where
  genValid = choose (-180_000_000_000, 180_000_000_000 -1) `suchThatMap` (mkLongitude . MkFixed)
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

instance GenValid OrganiserReminder where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Party where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEvent where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEventExport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PartyPoster where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Image where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Recurrence where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Schedule where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ScheduleParty where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddScheduleForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditScheduleForm where
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
