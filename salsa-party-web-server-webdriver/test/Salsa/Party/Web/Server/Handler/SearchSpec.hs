{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.TestImport
import Test.WebDriver as WD

spec :: WebdriverSpec
spec = do
  it "Can do a real search" $ \env ->
    forAll (elements locations) $ \Location {..} -> runWebdriverTestM env $ do
      openHome
      e <- WD.findElem (ById "query")
      WD.sendKeys (placeQuery locationPlace) e
      WD.findElem (ById "submit") >>= WD.submit
      route <- getCurrentRoute
      liftIO $ case route of
        SearchR loc -> loc `shouldBe` placeQuery locationPlace
        _ -> expectationFailure $ "Should have been at search results, but was at: " <> show route
