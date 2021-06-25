{-# LANGUAGE OverloadedStrings #-}
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
              sr <- searchQuery @IO day place
              liftIO $ sr `shouldBe` SearchResults {searchResultsParties = [], searchResultsExternalEvents = []}
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
                  DB.insert_ party3
                  sr <- searchQuery @IO day (placeCoordinates queryPlace)
                  liftIO $
                    sr
                      `shouldBe` SearchResults
                        { searchResultsParties =
                            [ (Entity party1Id party1, Entity place1Id place1, Nothing),
                              (Entity party2Id party2, Entity place2Id place2, Nothing)
                            ],
                          searchResultsExternalEvents = []
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
                  DB.insert_ party3
                  sr <- searchQuery @IO day (placeCoordinates queryPlace)
                  liftIO $
                    sr
                      `shouldBe` SearchResults
                        { searchResultsParties =
                            [ (Entity party1Id party1, Entity place1Id place1, Nothing),
                              (Entity party2Id party2, Entity place2Id place2, Nothing)
                            ],
                          searchResultsExternalEvents = []
                        }
      it "runs correctly with these two parties with a poster earch" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \partyPoster1Prototype ->
              forAllValid $ \partyPoster2Prototype ->
                forAllValid $ \image1Prototype ->
                  forAll (genValid `suchThat` (\i -> imageKey image1Prototype /= imageKey i)) $ \image2Prototype ->
                    forAllValid $ \day ->
                      flip runSqlPool pool $ do
                        let queryPlace = Place {placeQuery = "Search Place", placeLat = 0, placeLon = 0}
                        _ <- DB.insert queryPlace
                        let place1 = Place {placeQuery = "Place 1", placeLat = 0, placeLon = 0.1}
                        place1Id <- DB.insert place1
                        let place2 = Place {placeQuery = "Place 2", placeLat = 0.1, placeLon = 0}
                        place2Id <- DB.insert place2
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
                        image1Id <- DB.insert image1Prototype
                        DB.insert_ partyPoster1Prototype {partyPosterParty = party1Id, partyPosterImage = image1Id}
                        image2Id <- DB.insert image2Prototype
                        DB.insert_ partyPoster2Prototype {partyPosterParty = party2Id, partyPosterImage = image2Id}
                        sr <- searchQuery @IO day (placeCoordinates queryPlace)
                        liftIO $
                          sr
                            `shouldBe` SearchResults
                              { searchResultsParties =
                                  [ (Entity party1Id party1, Entity place1Id place1, Just $ imageKey image1Prototype),
                                    (Entity party2Id party2, Entity place2Id place2, Just $ imageKey image2Prototype)
                                  ],
                                searchResultsExternalEvents = []
                              }
      it "runs correctly with this complex case" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \party3Prototype ->
              forAllValid $ \externalEvent1Prototype ->
                forAll (genValid `suchThat` (\ee -> externalEventKey ee /= externalEventKey externalEvent1Prototype)) $ \externalEvent2Prototype ->
                  forAllValid $ \day ->
                    flip runSqlPool pool $ do
                      let queryPlace = Place {placeQuery = "Search Place", placeLat = 0, placeLon = 0}
                      _ <- DB.insert queryPlace
                      let place1 = Place {placeQuery = "Place 1", placeLat = 0, placeLon = 0.1}
                      place1Id <- DB.insert place1
                      let place2 = Place {placeQuery = "Place 2", placeLat = 0.1, placeLon = 0}
                      place2Id <- DB.insert place2
                      let place3 = Place {placeQuery = "Place 3", placeLat = 0.2, placeLon = 0.1}
                      place3Id <- DB.insert place3
                      let day2 = addDays 1 day
                      let party1 = party1Prototype {partyDay = day, partyPlace = place1Id}
                      party1Id <- DB.insert party1
                      let party2 = party2Prototype {partyDay = day2, partyPlace = place2Id}
                      DB.insert_ party2
                      let party3 = party3Prototype {partyDay = day2, partyPlace = place3Id}
                      DB.insert_ party3
                      let place4 = Place {placeQuery = "Place 4", placeLat = 0.1, placeLon = 0.2}
                      place4Id <- DB.insert place4
                      let place5 = Place {placeQuery = "Place 5", placeLat = 0.2, placeLon = 0.2}
                      place5Id <- DB.insert place5
                      let externalEvent1 = externalEvent1Prototype {externalEventDay = day, externalEventPlace = place4Id}
                      externalEvent1Id <- DB.insert externalEvent1
                      let externalEvent2 = externalEvent2Prototype {externalEventDay = day, externalEventPlace = place5Id}
                      externalEvent2Id <- DB.insert externalEvent2
                      sr <- searchQuery @IO day (placeCoordinates queryPlace)
                      liftIO $
                        sr
                          `shouldBe` SearchResults
                            { searchResultsParties =
                                [ (Entity party1Id party1, Entity place1Id place1, Nothing)
                                ],
                              searchResultsExternalEvents =
                                [ (Entity externalEvent1Id externalEvent1, Entity place4Id place4),
                                  (Entity externalEvent2Id externalEvent2, Entity place5Id place5)
                                ]
                            }
