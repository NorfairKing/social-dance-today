{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Event.Party.JSONSpec (spec) where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Password
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent
import Yesod.Core

spec :: Spec
spec = do
  genValidSpec @PartyExport
  jsonSpecOnValid @PartyExport

  serverSpec $ do
    describe "EventExportR" $ do
      it "Can get the json export for an existing party via the export route" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \party ->
            forAllValid $ \organiser ->
              forAllValid $ \user ->
                runYesodClientM yc $ do
                  testDB $ do
                    placeId <- DB.insert place
                    userId <- DB.insert user
                    organiserId <- DB.insert $ organiser {organiserUser = userId}
                    DB.insert_ $ party {partyPlace = placeId, partyOrganiser = organiserId}

                  get $ EventExportR $ partyUuid party
                  statusIs 200
                  mResp <- getResponse
                  case mResp of
                    Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                    Just resp -> do
                      let cts = responseBody resp
                      case JSON.eitherDecode cts of
                        Left err -> liftIO $ expectationFailure $ "Failed to parse JSON export:\n" <> err
                        Right export -> do
                          -- Clean database
                          testDB $ do
                            DB.deleteWhere ([] :: [DB.Filter Place])
                            DB.deleteWhere ([] :: [DB.Filter Organiser])
                            DB.deleteWhere ([] :: [DB.Filter User])
                            DB.deleteWhere ([] :: [DB.Filter Party])
                          -- Import the export
                          testDB $ do
                            Entity _ actualParty <- importPartyExport export
                            liftIO $
                              context (show cts) $
                                actualParty
                                  { partyPlace = partyPlace party,
                                    partyOrganiser = partyOrganiser party
                                  }
                                  `shouldBe` party
                            mPlace <- DB.get $ partyPlace actualParty
                            case mPlace of
                              Nothing -> liftIO $ expectationFailure "Should have found the place too"
                              Just actualPlace -> liftIO $ context (show cts) $ actualPlace `shouldBe` place

    describe "EventR" $ do
      it "Can get the json export for an existing party via an accept header" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \party ->
            forAllValid $ \organiser ->
              forAllValid $ \user ->
                runYesodClientM yc $ do
                  testDB $ do
                    placeId <- DB.insert place
                    userId <- DB.insert user
                    organiserId <- DB.insert $ organiser {organiserUser = userId}
                    DB.insert_ $ party {partyPlace = placeId, partyOrganiser = organiserId}
                  request $ do
                    setUrl $ EventR $ partyUuid party
                    addRequestHeader ("Accept", typeJson)
                  statusIs 200
                  mResp <- getResponse
                  case mResp of
                    Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                    Just resp -> do
                      let cts = responseBody resp
                      case JSON.eitherDecode cts of
                        Left err -> liftIO $ expectationFailure $ "Failed to parse JSON export:\n" <> err
                        Right export -> do
                          -- Clean database
                          testDB $ do
                            DB.deleteWhere ([] :: [DB.Filter Place])
                            DB.deleteWhere ([] :: [DB.Filter Organiser])
                            DB.deleteWhere ([] :: [DB.Filter User])
                            DB.deleteWhere ([] :: [DB.Filter Party])
                          -- Import the export
                          testDB $ do
                            Entity _ actualParty <- importPartyExport export
                            liftIO $
                              context (show cts) $
                                actualParty
                                  { partyPlace = partyPlace party,
                                    partyOrganiser = partyOrganiser party
                                  }
                                  `shouldBe` party
                            mPlace <- DB.get $ partyPlace actualParty
                            case mPlace of
                              Nothing -> liftIO $ expectationFailure "Should have found the place too"
                              Just actualPlace -> liftIO $ context (show cts) $ actualPlace `shouldBe` place

  dbSpec $ do
    describe "importPartyJSONExport" $
      it "roundtrips a party export" $ \pool ->
        forAllValid $ \party ->
          forAllValid $ \place ->
            forAllValid $ \organiser ->
              forAllValid $ \user ->
                runPersistentTest pool $ do
                  let export = partyExport party place organiser user
                  Entity _ party' <- importPartyExport export
                  liftIO $
                    context (ppShow export) $
                      party'
                        { partyPlace = partyPlace party,
                          partyOrganiser = partyOrganiser party
                        }
                        `shouldBe` party
                  mPlace <- DB.get $ partyPlace party'
                  case mPlace of
                    Nothing -> liftIO $ expectationFailure "Should have found the place too"
                    Just place' -> liftIO $ context (ppShow export) $ place' `shouldBe` place

  it "always outputs a valid export" $
    forAllValid $ \party ->
      forAllValid $ \place ->
        forAllValid $ \organiser ->
          forAllValid $ \user ->
            shouldBeValid $ partyExport party place organiser user

  it "outputs the same json export as before" $
    let exampleParty =
          Party
            { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
              partyTitle = "Bachata Community Zürich Mondays 💃🕺",
              partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags 💃🕺\n🕢 19:30 - 20:30 Warmup & Workshop\n🕣 20:30 - 23:30 Party\n📌Bürkliplatz Musikpavillon\nhttps://maps.app.goo.gl/JoTu9pabbsrHWXcZ7\n\n👍Start with Warmup and Musicality support\n\nPopular Song Wishes for dancing Bachateras and Bachateros 😊🎵\n\nKommst du auch mit uns tanzen?🕺💃",
              partyOrganiser = toSqlKey 0,
              partyDay = fromGregorian 2021 09 06,
              partyStart = Just (TimeOfDay 20 15 00),
              partyHomepage = Nothing,
              partyPrice = Just "15.0 CHF",
              partyCancelled = False,
              partyCreated = UTCTime (fromGregorian 2021 09 05) 185621,
              partyModified = Nothing,
              partyPlace = toSqlKey 0
            }

        examplePlace =
          Place
            { placeQuery = "Bahnhofplatz 6207 Nottwil LU",
              placeLat = Latitude 47.138657700,
              placeLon = Longitude 8.138471299
            }

        exampleOrganiser =
          Organiser
            { organiserUser = toSqlKey 0,
              organiserUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
              organiserName = "DJ Schenker🎵",
              organiserHomepage = Nothing,
              organiserCreated = UTCTime (fromGregorian 2021 09 01) 185621,
              organiserModified = Nothing
            }

        exampleUser =
          User
            { userEmailAddress = "marv.schenker@gmail.com",
              userPassphraseHash = PasswordHash "$2b$10$u3NwikkxT0bH778pEyQc6ONCwa1HkPpSLSbtI7kWbc/FPtJ4aesEe",
              userVerificationKey = Nothing,
              userCreated = UTCTime (fromGregorian 2021 09 01) 175621
            }

        export = partyExport exampleParty examplePlace exampleOrganiser exampleUser
     in pureGoldenByteStringFile "test_resources/json/party.json" $ LB.toStrict $ JSON.encode export
