{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.Web.Server.Handler.Search.QuerySpec (spec) where

import Control.Monad.Logger
import Data.Cache
import qualified Data.Map as M
import qualified Database.Esqueleto.Legacy as E
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Salsa.Party.Web.Server.Handler.Search.Query
import Salsa.Party.Web.Server.Handler.Search.Types
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent

spec :: Spec
spec = do
  dbSpec $ do
    modifyMaxSize (* 10) $
      modifyMaxSuccess (* 10) $
        describe "distanceEstimationQuery" $ do
          let helperQuery :: MonadIO m => Word -> Coordinates -> SqlPersistT m (Maybe (Entity Place))
              helperQuery maximumDistance coordinates = E.selectOne $
                E.from $ \place -> do
                  distanceEstimationQuery maximumDistance coordinates place
                  pure place

          let findsCloseEnoughPlaceTest :: DB.ConnectionPool -> Word -> Coordinates -> Place -> IO ()
              findsCloseEnoughPlaceTest pool maximumDistance coordinates place =
                runPersistentTest pool $ do
                  liftIO $ shouldBeValid coordinates
                  liftIO $ shouldBeValid place
                  DB.insert_ place
                  mPlace <- helperQuery maximumDistance coordinates
                  case mPlace of
                    Nothing ->
                      liftIO $
                        expectationFailure $
                          unlines
                            [ "Should have found place",
                              ppShow place,
                              "to be close (enough) to coordinates",
                              ppShow coordinates
                            ]
                    Just _ -> pure ()

          it "finds all places that are close enough" $ \pool -> do
            forAll (choose (minimumMaximumDistance, maximumMaximumDistance)) $ \maximumDistance ->
              forAllValid $ \coordinates ->
                forAllValid $ \place ->
                  when ((coordinates `distanceTo` placeCoordinates place) <= maximumDistance) $
                    findsCloseEnoughPlaceTest pool maximumDistance coordinates place

          it "works in zurich with the distance great enough to find Geneva" $ \pool ->
            findsCloseEnoughPlaceTest
              pool
              250_000 -- Distance is 222 km
              (Coordinates {coordinatesLat = Latitude 0, coordinatesLon = Longitude 0})
              (Place {placeLat = Latitude 2, placeLon = Longitude 0, placeQuery = "Somewhere else in the ocean"})

          it "works on the eastern boundary" $ \pool ->
            findsCloseEnoughPlaceTest
              pool
              100_000
              (Coordinates {coordinatesLat = Latitude 0, coordinatesLon = Longitude 179.99})
              (Place {placeLat = Latitude 0, placeLon = Longitude (-179.99), placeQuery = "over the eastern border"})

          it "works on the western boundary" $ \pool ->
            findsCloseEnoughPlaceTest
              pool
              100_000
              (Coordinates {coordinatesLat = Latitude 0, coordinatesLon = Longitude (-179.99)})
              (Place {placeLat = Latitude 0, placeLon = Longitude 179.99, placeQuery = "over the western border"})

          let doesNotFindCloseEnoughPlaceTest :: DB.ConnectionPool -> Word -> Coordinates -> Place -> IO ()
              doesNotFindCloseEnoughPlaceTest pool maximumDistance coordinates place =
                runPersistentTest pool $ do
                  liftIO $ shouldBeValid coordinates
                  liftIO $ shouldBeValid place
                  DB.insert_ place
                  mPlace <- helperQuery maximumDistance coordinates
                  case mPlace of
                    Nothing -> pure ()
                    -- Found it, must have been close enough
                    Just placeEntity ->
                      liftIO $
                        expectationFailure $
                          unlines
                            [ "Should not have found the place, but found this:",
                              ppShow (placeEntity :: Entity Place)
                            ]

          -- This _should_ not pass.
          -- That's why it's an 'Estimation' query.
          xit "only finds places that are close enough" $ \pool -> do
            forAllValid $ \maximumDistance ->
              forAllValid $ \coordinates ->
                forAllValid $ \place ->
                  doesNotFindCloseEnoughPlaceTest pool maximumDistance coordinates place

          it "works in zurich with the distance small enough to not find Geneva" $ \pool ->
            doesNotFindCloseEnoughPlaceTest
              pool
              100_000 -- Distance is 222 km
              (Coordinates {coordinatesLat = Latitude 0, coordinatesLon = Longitude 0})
              (Place {placeLat = Latitude 2, placeLon = Longitude 0, placeQuery = "Somewhere else in the ocean"})

    describe "searchQuery" $ do
      it "runs without results, and returns a map with empty days (and not an empty map)" $ \pool ->
        forAllValid $ \begin ->
          forAllValid $ \mEnd ->
            forAllValid $ \coordinates ->
              flip runSqlPool pool $ do
                sr <-
                  runUncachedSearchQueryForResults @IO
                    SearchQuery
                      { searchQueryBegin = begin,
                        searchQueryMEnd = mEnd,
                        searchQueryCoordinates = coordinates,
                        searchQueryDistance = Just defaultMaximumDistance,
                        searchQueryDanceStyle = Nothing
                      }
                liftIO $ sr `shouldBe` M.empty

      it "can figure out that we don't have data about this area, even we have data about other areas" $ \pool ->
        forAllValid $ \day ->
          forAllValid $ \organiser ->
            forAllValid $ \party1Prototype ->
              runNoLoggingT $
                flip runSqlPool pool $ do
                  let queryPlace = Place {placeQuery = "Search Place", placeLat = Latitude 0, placeLon = Longitude 0}
                  _ <- DB.insert queryPlace
                  let place1 = Place {placeQuery = "Place 1", placeLat = Latitude 45, placeLon = Longitude 0.05}
                  place1Id <- DB.insert place1
                  organiserId <- DB.insert organiser
                  let party1 =
                        party1Prototype
                          { partyOrganiser = organiserId,
                            partyDay = day,
                            partyPlace = place1Id
                          }
                  DB.insert_ party1
                  emptyCache <- liftIO $ newCache Nothing
                  sr <-
                    runSearchQuery @(NoLoggingT IO)
                      emptyCache
                      SearchQuery
                        { searchQueryBegin = day,
                          searchQueryMEnd = Just day,
                          searchQueryCoordinates = placeCoordinates queryPlace,
                          searchQueryDistance = Just defaultMaximumDistance,
                          searchQueryDanceStyle = Nothing
                        }
                  liftIO $ sr `shouldBe` NoDataYet

      it "runs correctly with these three parties where one is on a different day" $ \pool ->
        forAllValid $ \organiser ->
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
                    organiserId <- DB.insert organiser
                    let party1 =
                          party1Prototype
                            { partyOrganiser = organiserId,
                              partyDay = day,
                              partyPlace = place1Id
                            }
                    party1Id <- DB.insert party1
                    let party2 =
                          party2Prototype
                            { partyOrganiser = organiserId,
                              partyDay = day,
                              partyPlace = place2Id
                            }
                    party2Id <- DB.insert party2
                    -- close to party 1, but the next day
                    let party3 =
                          party3Prototype
                            { partyOrganiser = organiserId,
                              partyDay = addDays 1 day,
                              partyPlace = place3Id
                            }
                    DB.insert_ party3
                    sr <-
                      runUncachedSearchQueryForResults @IO
                        SearchQuery
                          { searchQueryBegin = day,
                            searchQueryMEnd = Just day,
                            searchQueryCoordinates = placeCoordinates queryPlace,
                            searchQueryDistance = Just defaultMaximumDistance,
                            searchQueryDanceStyle = Nothing
                          }
                    liftIO $
                      sr
                        `shouldBe` M.fromList
                          [ ( day,
                              [ Internal (Entity organiserId organiser) (Entity party1Id party1) (Entity place1Id place1) Nothing,
                                Internal (Entity organiserId organiser) (Entity party2Id party2) (Entity place2Id place2) Nothing
                              ]
                            )
                          ]

      it "runs correctly with these three parties where one is too far away" $ \pool ->
        forAllValid $ \organiser ->
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
                    organiserId <- DB.insert organiser
                    let party1 =
                          party1Prototype
                            { partyOrganiser = organiserId,
                              partyDay = day,
                              partyPlace = place1Id
                            }
                    party1Id <- DB.insert party1
                    let party2 =
                          party2Prototype
                            { partyOrganiser = organiserId,
                              partyDay = day,
                              partyPlace = place2Id
                            }
                    party2Id <- DB.insert party2
                    -- close to party 1, but the next day
                    let party3 =
                          party3Prototype
                            { partyOrganiser = organiserId,
                              partyDay = day,
                              partyPlace = place3Id
                            }
                    DB.insert_ party3
                    sr <-
                      runUncachedSearchQueryForResults @IO
                        SearchQuery
                          { searchQueryBegin = day,
                            searchQueryMEnd = Just day,
                            searchQueryCoordinates = placeCoordinates queryPlace,
                            searchQueryDistance = Just defaultMaximumDistance,
                            searchQueryDanceStyle = Nothing
                          }
                    liftIO $
                      sr
                        `shouldBe` M.fromList
                          [ ( day,
                              [ Internal (Entity organiserId organiser) (Entity party1Id party1) (Entity place1Id place1) Nothing,
                                Internal (Entity organiserId organiser) (Entity party2Id party2) (Entity place2Id place2) Nothing
                              ]
                            )
                          ]

      it "runs correctly with these two parties with a poster earch" $ \pool ->
        forAllValid $ \organiser ->
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
                          organiserId <- DB.insert organiser
                          let party1 =
                                party1Prototype
                                  { partyOrganiser = organiserId,
                                    partyDay = day,
                                    partyPlace = place1Id
                                  }
                          party1Id <- DB.insert party1
                          let party2 =
                                party2Prototype
                                  { partyOrganiser = organiserId,
                                    partyDay = day,
                                    partyPlace = place2Id
                                  }
                          party2Id <- DB.insert party2
                          image1Id <- DB.insert image1Prototype
                          DB.insert_ partyPoster1Prototype {partyPosterParty = party1Id, partyPosterImage = image1Id}
                          image2Id <- DB.insert image2Prototype
                          DB.insert_ partyPoster2Prototype {partyPosterParty = party2Id, partyPosterImage = image2Id}
                          sr <-
                            runUncachedSearchQueryForResults @IO
                              SearchQuery
                                { searchQueryBegin = day,
                                  searchQueryMEnd = Just day,
                                  searchQueryCoordinates = placeCoordinates queryPlace,
                                  searchQueryDistance = Just defaultMaximumDistance,
                                  searchQueryDanceStyle = Nothing
                                }
                          liftIO $
                            sr
                              `shouldBe` M.fromList
                                [ ( day,
                                    [ Internal (Entity organiserId organiser) (Entity party1Id party1) (Entity place1Id place1) (Just (imageKey image1Prototype)),
                                      Internal (Entity organiserId organiser) (Entity party2Id party2) (Entity place2Id place2) (Just (imageKey image2Prototype))
                                    ]
                                  )
                                ]

      it "runs correctly with this complex case" $ \pool ->
        forAllValid $ \organiser ->
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
                                organiserId <- DB.insert organiser
                                let party1 =
                                      party1Prototype
                                        { partyOrganiser = organiserId,
                                          partyTitle = "Party 1 abc",
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
                                let place4 = Place {placeQuery = "Place 4 lmn 😃", placeLat = Latitude 0.2, placeLon = Longitude (-0.2)}
                                place4Id <- DB.insert place4
                                let place5 = Place {placeQuery = "Place 5 opq", placeLat = Latitude (-0.2), placeLon = Longitude 0.2}
                                place5Id <- DB.insert place5
                                let place6 = Place {placeQuery = "Place 6 rst", placeLat = Latitude 0, placeLon = Longitude 0.1}
                                place6Id <- DB.insert place6
                                let place7 = Place {placeQuery = "Place 4 lmr", placeLat = Latitude 0.2, placeLon = Longitude (-0.2)}
                                place7Id <- DB.insert place7
                                let externalEvent1 =
                                      externalEvent1Prototype
                                        { externalEventTitle = "foo bar quux 😃",
                                          externalEventDay = day,
                                          externalEventPlace = place4Id,
                                          externalEventDescription = Nothing,
                                          externalEventOrganiser = Nothing,
                                          externalEventStart = Nothing,
                                          externalEventHomepage = Nothing,
                                          externalEventPrice = Nothing,
                                          externalEventCancelled = Just False
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
                                          externalEventOrganiser = Nothing,
                                          externalEventHomepage = Nothing,
                                          externalEventCancelled = Just False
                                        }
                                DB.insert_ externalEvent3
                                -- A duplicate of external event 1, not supposed to be shown
                                let externalEvent4 =
                                      externalEvent4Prototype
                                        { externalEventTitle = "foo bar quux",
                                          externalEventDay = day,
                                          externalEventPlace = place7Id,
                                          externalEventDescription = Nothing,
                                          externalEventOrganiser = Nothing,
                                          externalEventStart = Nothing,
                                          externalEventHomepage = Nothing,
                                          externalEventPrice = Nothing,
                                          externalEventCancelled = Just False
                                        }
                                DB.insert_ externalEvent4
                                sr <-
                                  runUncachedSearchQueryForResults @IO
                                    SearchQuery
                                      { searchQueryBegin = day,
                                        searchQueryMEnd = Just day,
                                        searchQueryCoordinates = placeCoordinates queryPlace,
                                        searchQueryDistance = Just defaultMaximumDistance,
                                        searchQueryDanceStyle = Nothing
                                      }
                                liftIO $
                                  sr
                                    `shouldBe` M.fromList
                                      [ ( day,
                                          [ Internal (Entity organiserId organiser) (Entity party1Id party1) (Entity place1Id place1) Nothing,
                                            External (Entity externalEvent1Id externalEvent1) (Entity place4Id place4) Nothing,
                                            External (Entity externalEvent2Id externalEvent2) (Entity place5Id place5) Nothing
                                          ]
                                        )
                                      ]
