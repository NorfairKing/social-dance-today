{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTestSpec (spec) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $
  describe "AdminR" $
    describe "SiteTestR" $ do
      it "GETs a 200 when logged in as admin" $
        withLoggedInAdmin $ do
          get $ AdminR AdminSiteTestR
          statusIs 200

      it "POSTs a 200 when logged in as admin and testing localhost" $
        withLoggedInAdmin $ do
          get $ AdminR AdminSiteTestR
          uri <- asks yesodClientSiteURI
          request $ do
            setUrl $ AdminR AdminSiteTestR
            setMethod methodPost
            addToken
            addPostParam "url" $ T.pack $ show uri
          statusIs 200
