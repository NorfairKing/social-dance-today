{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Event.JSON.PlaceSpec (spec) where

import Salsa.Party.Web.Server.Handler.Event.JSON.Place
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent

spec :: Spec
spec = do
  genValidSpec @PlaceExport
  jsonSpecOnValid @PlaceExport
  modifyMaxSize (* 10) $
    dbSpec $ do
      describe "importPlaceExport" $ do
        it "roundtrips a place export" $ \pool -> do
          forAllValid $ \place -> runPersistentTest pool $ do
            let export = placeExport place
            Entity _ place' <- importPlaceExport export
            liftIO $ context (ppShow export) $ place' `shouldBe` place
