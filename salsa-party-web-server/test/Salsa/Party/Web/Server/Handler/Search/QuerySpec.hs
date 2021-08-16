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
                      forAllValid $ \day ->
                        flip runSqlPool pool $ do
                          let queryPlace = Place {placeQuery = "Search Place", placeLat = 0, placeLon = 0}
                          _ <- DB.insert queryPlace
                          let place1 = Place {placeQuery = "Place 1 abc", placeLat = 0, placeLon = 0.1}
                          place1Id <- DB.insert place1
                          let place2 = Place {placeQuery = "Place 2 def", placeLat = 0.1, placeLon = 0}
                          place2Id <- DB.insert place2
                          let place3 = Place {placeQuery = "Place 3 ghi", placeLat = 0.2, placeLon = 0.1}
                          place3Id <- DB.insert place3
                          let day2 = addDays 1 day
                          let party1 = party1Prototype {partyTitle = "Party 1 abc", partyDay = day, partyPlace = place1Id}
                          party1Id <- DB.insert party1
                          let party2 = party2Prototype {partyTitle = "Party 2 def", partyDay = day2, partyPlace = place2Id}
                          DB.insert_ party2
                          let party3 = party3Prototype {partyTitle = "Party 3 ghi", partyDay = day2, partyPlace = place3Id}
                          DB.insert_ party3
                          let place4 = Place {placeQuery = "Place 4 lmn", placeLat = 0.1, placeLon = 0.2}
                          place4Id <- DB.insert place4
                          let place5 = Place {placeQuery = "Place 5 opq", placeLat = 0.2, placeLon = 0.2}
                          place5Id <- DB.insert place5
                          let place6 = Place {placeQuery = "Place 6 rst", placeLat = 0.2, placeLon = 0.2}
                          place6Id <- DB.insert place6
                          let externalEvent1 = externalEvent1Prototype {externalEventTitle = "External Event 1 abcdef", externalEventDay = day, externalEventPlace = place4Id}
                          externalEvent1Id <- DB.insert externalEvent1
                          let externalEvent2 = externalEvent2Prototype {externalEventTitle = "External Event 2 ghijkl", externalEventDay = day, externalEventPlace = place5Id}
                          externalEvent2Id <- DB.insert externalEvent2
                          -- A duplicate of party 1, not supposed to be shown
                          let externalEvent3 = externalEvent3Prototype {externalEventTitle = "Party 1 abcd", externalEventDay = day, externalEventPlace = place6Id}
                          _ <- DB.insert externalEvent3
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

  describe "closeEnoughTo" $ do
    it "Zouk party" $
      closeEnoughTo
        "Zouk meets Bachata @Bürkliplatz 😊🎵"
        "ZOUK meets BACHATA Party @Bürkliplatz 😊🎵"
    it "Comma in address" $
      closeEnoughTo
        "Viaduktstrasse 67, 8005 Zürich"
        "Viaduktstrasse 67 8005 Zürich"
    it "Extra letter somewhere" $
      closeEnoughTo
        "Bachata Community Zürich Monday 💃🕺"
        "Bachata Community Zürich Mondays 💃🕺"
    it "Different casing" $
      closeEnoughTo
        "Noche Latina mit Powell und DJ Ñoño"
        "NOCHE LATINA - mit Powell und DJ Ñoño"
    it "Extra nonsense" $
      closeEnoughTo
        "Bachateros Treff"
        "BACHATEROS TREFF ★★★★★"
    it "Completely different" $
      not $
        closeEnoughTo
          "Bachata Community Zürich Monday 💃🕺"
          "Lounge@Bananenreiferei"
    it "Completely different" $
      not $
        closeEnoughTo
          "Social Salsa Party"
          "Free workshops!"
    it "Close but different" $
      not $
        closeEnoughTo
          "Syd's birthday party"
          "Josh's birthday party"
    it "Completely different but all symbols" $
      not $
        closeEnoughTo
          "平仮名"
          "漢字"
