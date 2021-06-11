{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist (Entity (..), (=.))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Network.HTTP.Client as HTTP
import Path.IO
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.Party
import Salsa.Party.Web.Server.Static
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod
import Yesod (Textarea (..))

type ServerSpec = YesodSpec App

serverSpec :: ServerSpec -> Spec
serverSpec = modifyMaxSuccess (`div` 20) . managerSpec . yesodSpecWithSiteSetupFunc serverSetupFunc

serverSetupFunc :: HTTP.Manager -> SetupFunc App
serverSetupFunc man = do
  tdir <- tempDirSetupFunc "salsa"
  pool <- connectionPoolSetupFunc migrateAll
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  pure
    App
      { appLogLevel = LevelWarn,
        appStatic = salsaPartyWebServerStatic,
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

type DBSpec = SpecWith DB.ConnectionPool

dbSpec :: DBSpec -> Spec
dbSpec = modifyMaxSuccess (`div` 10) . persistSqliteSpec migrateAll

testSubmitPlace :: Text -> Coordinates -> YesodClientM App (Entity Place)
testSubmitPlace address Coordinates {..} =
  testDB $
    DB.upsertBy
      (UniquePlaceQuery address)
      ( Place
          { placeLat = coordinatesLat,
            placeLon = coordinatesLon,
            placeQuery = address
          }
      )
      [ PlaceLat =. coordinatesLat,
        PlaceLon =. coordinatesLon
      ]

testSubmitParty :: PartyForm -> Coordinates -> YesodClientM App PartyId
testSubmitParty PartyForm {..} loc = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  _ <- testSubmitPlace partyFormAddress loc
  get SubmitPartyR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl SubmitPartyR
    addToken
    addPostParam "title" partyFormTitle
    addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" partyFormDay
    addPostParam "address" partyFormAddress
    forM_ partyFormDescription $ \description -> addPostParam "description" $ unTextarea description
    forM_ partyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      PartyR partyId -> pure partyId
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some PartyR after submitting a party, was this instead: " <> show redirectLocation

testDB :: DB.SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
