{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Search.QuerySpec (spec) where

import qualified Data.Map as M
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  dbSpec $ do
    describe "searchQuery" $ do
      it "runs without results, and returns a map with empty days (and not an empty map)" $ \pool ->
        forAllValid $ \begin ->
          forAllValid $ \mEnd ->
            forAllValid $ \place ->
              flip runSqlPool pool $ do
                sr <- searchQuery @IO begin mEnd place
                liftIO $ sr `shouldBe` M.empty

      it "runs correctly with these three parties where one is on a different day" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \party3Prototype ->
              forAllValid $ \day ->
                flip runSqlPool pool $ do
                  let queryPlace = Place {placeQuery = "Search Place", placeLat = Latitude 0, placeLon = Longitude 0}
                  _ <- DB.insert queryPlace
                  let place1 = Place {placeQuery = "Place 1", placeLat = Latitude 0, placeLon = Longitude 0.05}
                  place1Id <- DB.insert place1
                  let place2 = Place {placeQuery = "Place 2", placeLat = Latitude 0, placeLon = Longitude 0.1}
                  place2Id <- DB.insert place2
                  let place3 = Place {placeQuery = "Place 3", placeLat = Latitude 0, placeLon = Longitude 0.15}
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
                  sr <- searchQuery @IO day (Just day) (placeCoordinates queryPlace)
                  liftIO $
                    sr
                      `shouldBe` M.fromList
                        [ ( day,
                            [ Internal (Entity party1Id party1) (Entity place1Id place1) Nothing,
                              Internal (Entity party2Id party2) (Entity place2Id place2) Nothing
                            ]
                          )
                        ]

      it "runs correctly with these three parties where one is too far away" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \party3Prototype ->
              forAllValid $ \day ->
                flip runSqlPool pool $ do
                  let queryPlace = Place {placeQuery = "Search Place", placeLat = Latitude 0, placeLon = Longitude 0}
                  _ <- DB.insert queryPlace
                  let place1 = Place {placeQuery = "Place 1", placeLat = Latitude 0, placeLon = Longitude 0.1}
                  place1Id <- DB.insert place1
                  let place2 = Place {placeQuery = "Place 2", placeLat = Latitude 0.1, placeLon = Longitude 0}
                  place2Id <- DB.insert place2
                  let place3 = Place {placeQuery = "Place 3", placeLat = Latitude 5, placeLon = Longitude 5}
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
                  sr <- searchQuery @IO day (Just day) (placeCoordinates queryPlace)
                  liftIO $
                    sr
                      `shouldBe` M.fromList
                        [ ( day,
                            [ Internal (Entity party1Id party1) (Entity place1Id place1) Nothing,
                              Internal (Entity party2Id party2) (Entity place2Id place2) Nothing
                            ]
                          )
                        ]

      it "runs correctly with these two parties with a poster earch" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \partyPoster1Prototype ->
              forAllValid $ \partyPoster2Prototype ->
                forAllValid $ \image1Prototype ->
                  forAll (genValid `suchThat` (\i -> imageKey image1Prototype /= imageKey i)) $ \image2Prototype ->
                    forAllValid $ \day ->
                      flip runSqlPool pool $ do
                        let queryPlace = Place {placeQuery = "Search Place", placeLat = Latitude 0, placeLon = Longitude 0}
                        _ <- DB.insert queryPlace
                        let place1 = Place {placeQuery = "Place 1", placeLat = Latitude 0, placeLon = Longitude 0.1}
                        place1Id <- DB.insert place1
                        let place2 = Place {placeQuery = "Place 2", placeLat = Latitude 0.1, placeLon = Longitude 0}
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
                        sr <- searchQuery @IO day (Just day) (placeCoordinates queryPlace)
                        liftIO $
                          sr
                            `shouldBe` M.fromList
                              [ ( day,
                                  [ Internal (Entity party1Id party1) (Entity place1Id place1) (Just (imageKey image1Prototype)),
                                    Internal (Entity party2Id party2) (Entity place2Id place2) (Just (imageKey image2Prototype))
                                  ]
                                )
                              ]

      it "runs correctly with this complex case" $ \pool ->
        forAllValid $ \party1Prototype ->
          forAllValid $ \party2Prototype ->
            forAllValid $ \party3Prototype ->
              forAllValid $ \externalEvent1Prototype ->
                forAll (genValid `suchThat` (\ee -> externalEventKey ee /= externalEventKey externalEvent1Prototype)) $ \externalEvent2Prototype ->
                  forAll
                    ( genValid
                        `suchThat` ( \ee ->
                                       externalEventKey ee /= externalEventKey externalEvent1Prototype
                                         && externalEventKey ee /= externalEventKey externalEvent2Prototype
                                   )
                    )
                    $ \externalEvent3Prototype ->
                      forAll
                        ( genValid
                            `suchThat` ( \ee ->
                                           externalEventKey ee /= externalEventKey externalEvent1Prototype
                                             && externalEventKey ee /= externalEventKey externalEvent2Prototype
                                             && externalEventKey ee /= externalEventKey externalEvent3Prototype
                                       )
                        )
                        $ \externalEvent4Prototype ->
                          forAllValid $ \day ->
                            flip runSqlPool pool $ do
                              let queryPlace = Place {placeQuery = "Search Place", placeLat = Latitude 0, placeLon = Longitude 0}
                              _ <- DB.insert queryPlace
                              let place1 = Place {placeQuery = "Place 1 abc", placeLat = Latitude 0, placeLon = Longitude 0.1}
                              place1Id <- DB.insert place1
                              let place2 = Place {placeQuery = "Place 2 def", placeLat = Latitude 0.1, placeLon = Longitude 0}
                              place2Id <- DB.insert place2
                              let place3 = Place {placeQuery = "Place 3 ghi", placeLat = Latitude 0.2, placeLon = Longitude 0.1}
                              place3Id <- DB.insert place3
                              let otherDay = addDays 1 day
                              let party1 =
                                    party1Prototype
                                      { partyTitle = "Party 1 abc",
                                        partyDay = day,
                                        partyPlace = place1Id,
                                        partyDescription = Nothing,
                                        partyCancelled = False,
                                        partyHomepage = Nothing
                                      }
                              party1Id <- DB.insert party1
                              let party2 = party2Prototype {partyTitle = "Party 2 def", partyDay = otherDay, partyPlace = place2Id}
                              DB.insert_ party2
                              let party3 = party3Prototype {partyTitle = "Party 3 ghi", partyDay = otherDay, partyPlace = place3Id}
                              DB.insert_ party3
                              let place4 = Place {placeQuery = "Place 4 lmn", placeLat = Latitude 0.2, placeLon = Longitude (-0.2)}
                              place4Id <- DB.insert place4
                              let place5 = Place {placeQuery = "Place 5 opq", placeLat = Latitude (-0.2), placeLon = Longitude 0.2}
                              place5Id <- DB.insert place5
                              let place6 = Place {placeQuery = "Place 6 rst", placeLat = Latitude 0, placeLon = Longitude 0.1}
                              place6Id <- DB.insert place6
                              let place7 = Place {placeQuery = "Place 7 pqr", placeLat = Latitude 0.2, placeLon = Longitude (-0.2)}
                              place7Id <- DB.insert place7
                              let externalEvent1 =
                                    externalEvent1Prototype
                                      { externalEventTitle = "foo bar quux",
                                        externalEventDay = day,
                                        externalEventPlace = place4Id,
                                        externalEventDescription = Nothing,
                                        externalEventCancelled = False
                                      }
                              externalEvent1Id <- DB.insert externalEvent1
                              let externalEvent2 =
                                    externalEvent2Prototype
                                      { externalEventTitle = "ellemenopeee abcdefghijklmnope",
                                        externalEventDay = day,
                                        externalEventPlace = place5Id
                                      }
                              externalEvent2Id <- DB.insert externalEvent2
                              -- A duplicate of party 1, not supposed to be shown
                              let externalEvent3 =
                                    externalEvent3Prototype
                                      { externalEventTitle = "Party 1 abcd",
                                        externalEventDay = day,
                                        externalEventPlace = place6Id,
                                        externalEventDescription = Nothing,
                                        externalEventHomepage = Nothing,
                                        externalEventCancelled = False
                                      }
                              DB.insert_ externalEvent3
                              -- A duplicate of external event 1, not supposed to be shown
                              let externalEvent4 =
                                    externalEvent4Prototype
                                      { externalEventTitle = "foo bar quux",
                                        externalEventDay = day,
                                        externalEventPlace = place7Id,
                                        externalEventDescription = Nothing,
                                        externalEventCancelled = False
                                      }
                              DB.insert_ externalEvent4
                              sr <- searchQuery @IO day (Just day) (placeCoordinates queryPlace)
                              liftIO $
                                sr
                                  `shouldBe` M.fromList
                                    [ ( day,
                                        [ Internal (Entity party1Id party1) (Entity place1Id place1) Nothing,
                                          External (Entity externalEvent1Id externalEvent1) (Entity place4Id place4) Nothing,
                                          External (Entity externalEvent2Id externalEvent2) (Entity place5Id place5) Nothing
                                        ]
                                      )
                                    ]
