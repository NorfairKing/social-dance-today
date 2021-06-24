{-# LANGUAGE RecordWildCards #-}
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
import Network.URI
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

instance GenValid Organiser where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Party where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEvent where
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

instance GenValid URI where
  shrinkValid _ = [] -- No point yet
  genValid = do
    let uriScheme = "https:"
    uriAuthority <- genValid
    pathLen <- upTo 10
    p <- replicateM (max 1 pathLen) $ elements $ '/' : ['a' .. 'z']
    let uriPath = if not (null p) then '/' : p else ""
    queryLen <- upTo 10
    q <- replicateM (max 1 queryLen) $ elements ['a' .. 'z']
    let uriQuery = if not (null q) then '?' : q else ""
    fragLen <- upTo 10
    f <- replicateM (max 1 fragLen) $ elements ['a' .. 'z']
    let uriFragment = if not (null f) then '#' : f else ""
    pure URI {..}

instance GenValid URIAuth where
  shrinkValid _ = [] -- No point yet.
  genValid = do
    let uriUserInfo = ""
    domainLen <- upTo 10
    domain <- replicateM (max 1 domainLen) $ choose ('a', 'z')
    tldLen <- upTo 10
    tld <- replicateM (max 1 tldLen) $ choose ('a', 'z')
    let uriRegName = concat [domain, ".", tld]
    let uriPort = ""
    pure URIAuth {..}
