{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Gen where

import Conduit
import Control.Monad
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Organiser
import Salsa.Party.Web.Server.Handler.Party
import Test.QuickCheck
import Yesod.Core.Types

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid = fmap Textarea . shrinkValid . unTextarea

instance GenValid FileInfo where
  shrinkValid _ = [] -- No point
  genValid = do
    FileInfo
      <$> genValid
      <*> genValid
      <*> (yield <$> genValid)
      <*> pure (\_ -> pure ()) -- We really shouldn't be generating this ..?

instance GenValid OrganiserForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Coordinates where
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

-- FIXME this isn't very general
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
