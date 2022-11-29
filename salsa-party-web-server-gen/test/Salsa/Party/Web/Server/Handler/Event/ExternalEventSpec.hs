{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEventSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Search (daysToKeepPartiesMarkedAsAvailable)
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  serverSpec $ do
    describe "EventR" $ do
      it "Gets a 404 or 410 for a nonexistent external event by uuid" $ \yc ->
        forAllValid $ \uuid ->
          runYesodClientM yc $ do
            get $ EventR uuid
            s <- requireStatus
            case s of
              404 -> pure ()
              410 -> pure ()
              _ -> liftIO $ expectationFailure $ show s

      it "Can get the party page for an existing external event by uuid" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              get $ EventR $ externalEventUuid externalEvent
              _ <- followRedirect
              statusIs 200
              shouldHaveNoNoArchiveXRobotsTag
              shouldHaveUnavailableAfterXRobotsTag (addDays daysToKeepPartiesMarkedAsAvailable (externalEventDay externalEvent))

      it "Gets a 404 or 410 for a nonexistent external event by slug" $ \yc ->
        forAllValid $ \slug ->
          forAllValid $ \day ->
            runYesodClientM yc $ do
              get $ ExternalEventSlugR slug day
              s <- requireStatus
              case s of
                404 -> pure ()
                410 -> pure ()
                _ -> liftIO $ expectationFailure $ show s

      it "Can get the party page for an existing external event by slug" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            case externalEventSlugRoute externalEvent of
              Nothing -> pure ()
              Just route -> do
                runYesodClientM yc $ do
                  testDB $ do
                    placeId <- DB.insert place
                    DB.insert_ $ externalEvent {externalEventPlace = placeId}
                  get route
                  statusIs 200
                  shouldHaveNoNoArchiveXRobotsTag
                  shouldHaveUnavailableAfterXRobotsTag (addDays daysToKeepPartiesMarkedAsAvailable (externalEventDay externalEvent))
