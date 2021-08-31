{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Organiser.TestUtils where

import Control.Monad.Reader
import Salsa.Party.DB
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Test.Syd
import Test.Syd.Yesod

testSubmitOrganiser :: OrganiserForm -> YesodClientM App ()
testSubmitOrganiser OrganiserForm {..} = do
  let OrganiserForm _ _ _ = undefined
  get $ AccountR AccountOrganiserR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AccountR AccountOrganiserR
    addToken
    addPostParam "name" organiserFormName
    forM_ organiserFormHomepage $ \homepage -> addPostParam "homepage" homepage
    when organiserFormConsentReminder $ addPostParam "reminder-consent" "on"
  statusIs 303
  locationShouldBe $ AccountR AccountOrganiserR
  _ <- followRedirect
  statusIs 200

organiserFormShouldMatch :: OrganiserForm -> Organiser -> IO ()
organiserFormShouldMatch OrganiserForm {..} Organiser {..} = do
  let OrganiserForm _ _ _ = undefined -- We want to check every part of the party form
  context "name" $ organiserName `shouldBe` organiserFormName
  context "homepage" $ organiserHomepage `shouldBe` organiserFormHomepage
  pure ()
