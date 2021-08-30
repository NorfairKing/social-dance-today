{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.TestImport
import Test.WebDriver as WD

spec :: WebdriverSpec
spec =
  it "Can do a real search" $ \env ->
    forAll (elements locations) $ \Location {..} -> runWebdriverTestM env $ do
      openHome
      e <- WD.findElem (ById "queryInput")
      WD.sendKeys (placeQuery locationPlace) e
      WD.submit e
