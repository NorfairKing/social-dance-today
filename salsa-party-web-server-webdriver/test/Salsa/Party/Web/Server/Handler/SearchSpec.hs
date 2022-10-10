{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.TestImport
import Test.WebDriver as WD

spec :: WebdriverSpec App
spec = do
  it "Can do a real search on the homepage" $ \env ->
    forAll (elements locations) $ \Location {..} -> runWebdriverTestM env $ do
      openHome
      e <- WD.findElem (ById "query")
      WD.sendKeys (placeQuery locationPlace) e
      WD.findElem (ById "submit") >>= WD.submit
      route <- getCurrentRoute
      liftIO $ case route of
        SearchR loc -> loc `shouldBe` placeQuery locationPlace
        _ -> expectationFailure $ "Should have been at search results, but was at: " <> show route
  it "Can do a real advanced search" $ \env ->
    forAll (elements locations) $ \Location {..} -> runWebdriverTestM env $ do
      openRoute AdvancedSearchR
      driveAdvancedSearch
        QueryForm
          { queryFormAddress = Just $ placeQuery locationPlace,
            queryFormCoordinates = Nothing,
            queryFormBegin = Just $ fromGregorian 2021 09 18,
            queryFormEnd = Just $ fromGregorian 2021 09 21,
            queryFormOn = Nothing,
            queryFormDistance = Just 20,
            queryFormDanceStyle = Nothing
          }
      route <- getCurrentRoute
      liftIO $ case route of
        QueryR -> pure ()
        _ -> expectationFailure $ "Should have been at QueryR, but was at: " <> show route
