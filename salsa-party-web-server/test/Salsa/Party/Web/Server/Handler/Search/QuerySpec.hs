{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Search.QuerySpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  dbSpec $ do
    describe "searchQuery" $ do
      it "runs without results" $ \pool ->
        forAllValid $ \day ->
          forAllValid $ \place ->
            flip runSqlPool pool $ do
              SearchResults {..} <- searchQuery @IO day place
              liftIO $ searchResultsParties `shouldBe` []
      it "runs correctly with these three parties where one is on a different day" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \party3Prototype ->
              forAllValid $ \day ->
                flip runSqlPool pool $ do
                  let queryPlace = Place {placeQuery = "Search Place", placeLat = 0, placeLon = 0}
                  _ <- DB.insert queryPlace
                  let place1 = Place {placeQuery = "Place 1", placeLat = 0, placeLon = 0.05}
                  place1Id <- DB.insert place1
                  let place2 = Place {placeQuery = "Place 2", placeLat = 0, placeLon = 0.1}
                  place2Id <- DB.insert place2
                  let place3 = Place {placeQuery = "Place 3", placeLat = 0, placeLon = 0.15}
                  place3Id <- DB.insert place3
                  let party1 =
                        party1Prototype
                          { partyDay = day,
                            partyPlace = place1Id
                          }
                  party1Id <- DB.insert party1
                  let party2 =
                        party2Prototype
                          { partyDay = day,
                            partyPlace = place2Id
                          }
                  party2Id <- DB.insert party2
                  -- close to party 1, but the next day
                  let party3 =
                        party3Prototype
                          { partyDay = addDays 1 day,
                            partyPlace = place3Id
                          }
                  _ <- DB.insert party3
                  sr <- searchQuery @IO day (placeCoordinates queryPlace)
                  liftIO $
                    sr
                      `shouldBe` SearchResults
                        { searchResultsParties =
                            [ (Entity party1Id party1, Entity place1Id place1, Nothing),
                              (Entity party2Id party2, Entity place2Id place2, Nothing)
                            ]
                        }
      it "runs correctly with these three parties where one is too far away" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \party3Prototype ->
              forAllValid $ \day ->
                flip runSqlPool pool $ do
                  let queryPlace = Place {placeQuery = "Search Place", placeLat = 0, placeLon = 0}
                  _ <- DB.insert queryPlace
                  let place1 = Place {placeQuery = "Place 1", placeLat = 0, placeLon = 0.1}
                  place1Id <- DB.insert place1
                  let place2 = Place {placeQuery = "Place 2", placeLat = 0.1, placeLon = 0}
                  place2Id <- DB.insert place2
                  let place3 = Place {placeQuery = "Place 3", placeLat = 5, placeLon = 5}
                  place3Id <- DB.insert place3
                  let party1 =
                        party1Prototype
                          { partyDay = day,
                            partyPlace = place1Id
                          }
                  party1Id <- DB.insert party1
                  let party2 =
                        party2Prototype
                          { partyDay = day,
                            partyPlace = place2Id
                          }
                  party2Id <- DB.insert party2
                  -- close to party 1, but the next day
                  let party3 =
                        party3Prototype
                          { partyDay = day,
                            partyPlace = place3Id
                          }
                  _ <- DB.insert party3
                  sr <- searchQuery @IO day (placeCoordinates queryPlace)
                  liftIO $
                    sr
                      `shouldBe` SearchResults
                        { searchResultsParties =
                            [ (Entity party1Id party1, Entity place1Id place1, Nothing),
                              (Entity party2Id party2, Entity place2Id place2, Nothing)
                            ]
                        }
