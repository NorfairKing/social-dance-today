{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Charts
  ( getAdminChartsR,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word
import Safe
import Salsa.Party.Web.Server.Handler.Import
import Text.Julius
import Text.Printf

getAdminChartsR :: Handler Html
getAdminChartsR = do
  today <- getClientToday
  acqExternalEventsSource <- runDB $ selectSourceRes [ExternalEventDay >=. addDays (-daysToKeepPartiesMarkedAsAvailable + 1) today] [Asc ExternalEventId]
  acqPartiesSource <- runDB $ selectSourceRes [] [Asc PartyId]

  -- Upcoming events
  dayCountMapOfExternalEvents <- makeUpcomingEventsMap today acqExternalEventsSource $ \(Entity _ ExternalEvent {..}) ->
    (utctDay externalEventCreated, externalEventDay)

  dayCountMapOfParties <- makeUpcomingEventsMap today acqPartiesSource $ \(Entity _ Party {..}) ->
    (utctDay partyCreated, partyDay)

  let dayCountMapOfTotal = combineUpcomingEventsMap dayCountMapOfParties dayCountMapOfExternalEvents

  dayCountMapOfExternalEvents30Days <- makeUpcomingEventsMapWithLimit today 30 acqExternalEventsSource $ \(Entity _ ExternalEvent {..}) ->
    (utctDay externalEventCreated, externalEventDay)

  dayCountMapOfParties30Days <- makeUpcomingEventsMapWithLimit today 30 acqPartiesSource $ \(Entity _ Party {..}) ->
    (utctDay partyCreated, partyDay)

  let dayCountMapOfTotal30Days = combineUpcomingEventsMap dayCountMapOfParties30Days dayCountMapOfExternalEvents30Days

  dayCountMapOfExternalEvents7Days <- makeUpcomingEventsMapWithLimit today 7 acqExternalEventsSource $ \(Entity _ ExternalEvent {..}) ->
    (utctDay externalEventCreated, externalEventDay)

  dayCountMapOfParties7Days <- makeUpcomingEventsMapWithLimit today 7 acqPartiesSource $ \(Entity _ Party {..}) ->
    (utctDay partyCreated, partyDay)

  let dayCountMapOfTotal7Days = combineUpcomingEventsMap dayCountMapOfParties7Days dayCountMapOfExternalEvents7Days

  -- Users and organisers
  acqUsersSource <- runDB $ selectSourceRes [] [Asc UserId]
  dayCountOfUsers <- makeDayCountMap today acqUsersSource $ utctDay . userCreated . entityVal
  acqOrganisersSource <- runDB $ selectSourceRes [] [Asc OrganiserId]
  dayCountOfOrganisers <- makeDayCountMap today acqOrganisersSource $ utctDay . organiserCreated . entityVal

  -- Per day
  partiesPerDayMap <- makePerDayCountMap acqPartiesSource $ partyDay . entityVal
  externalEventsPerDayMap <- makePerDayCountMap acqExternalEventsSource $ externalEventDay . entityVal
  let eventsPerDayMap = M.unionWith (+) partiesPerDayMap externalEventsPerDayMap

  let minDay = fromMaybe today $ minimumMay $ map fst $ mapMaybe M.lookupMin [dayCountMapOfExternalEvents, dayCountMapOfParties]
      minPartyDay = addDays (-daysToKeepPartiesMarkedAsAvailable + 1) today
      curDay = today

  withNavBar $ do
    addScriptRemote "https://cdn.jsdelivr.net/npm/chart.js@3.4.1/dist/chart.min.js"
    $(widgetFile "admin/charts")

currentAndLastMonthCountWidget :: Map Day Word64 -> Widget
currentAndLastMonthCountWidget m = do
  today <- getClientToday
  let nowCount :: Word64
      nowCount = fromMaybe 0 $ M.lookup today m
  let lastMonthCount :: Word64
      lastMonthCount = fromMaybe 0 $ M.lookup (addDays (-30) today) m
  let increaseRatio :: Double
      increaseRatio = (fromIntegral nowCount - fromIntegral lastMonthCount) / fromIntegral lastMonthCount
  let increasePercentageString :: String
      increasePercentageString = printf "%+.0f" (increaseRatio * 100)
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
  [whamlet|
     #{nowCount}, #{increasePercentageString}%
  |]

makePerDayCountMap :: MonadUnliftIO m => Acquire (ConduitT () a m ()) -> (a -> Day) -> m (Map Day Word64)
makePerDayCountMap acqSource func = withAcquire acqSource $ \source ->
  runConduit $ source .| C.foldl (\m a -> addToPerDayCountMap (func a) m) M.empty

addToPerDayCountMap :: Day -> Map Day Word64 -> Map Day Word64
addToPerDayCountMap = M.alter go
  where
    go :: Maybe Word64 -> Maybe Word64
    go = \case
      Nothing -> Just 1
      Just n -> Just $ succ n

makeDayCountMap :: MonadUnliftIO m => Day -> Acquire (ConduitT () a m ()) -> (a -> Day) -> m (Map Day Word64)
makeDayCountMap today source func = makeDayMap source (\m a -> addToDayMap today m (func a))

addToDayMap :: Day -> Map Day Word64 -> Day -> Map Day Word64
addToDayMap today m created =
  let days = [created .. today]
      mapOfOnes = M.fromAscList $ map (\d -> (d, 1)) days
   in M.unionWith (+) mapOfOnes m

combineUpcomingEventsMap :: Map Day Word64 -> Map Day Word64 -> Map Day Word64
combineUpcomingEventsMap = M.unionWith (+)

makeUpcomingEventsMap :: MonadUnliftIO m => Day -> Acquire (ConduitT () a m ()) -> (a -> (Day, Day)) -> m (Map Day Word64)
makeUpcomingEventsMap today source func =
  makeDayMap
    source
    ( \m a ->
        let (created, scheduled) = func a
         in addToUpcomingEventsMap today m created scheduled
    )

addToUpcomingEventsMap :: Day -> Map Day Word64 -> Day -> Day -> Map Day Word64
addToUpcomingEventsMap today m created scheduled =
  let days = [created .. min today scheduled]
      mapOfOnes = M.fromAscList $ map (\d -> (d, 1)) days
   in M.unionWith (+) mapOfOnes m

makeUpcomingEventsMapWithLimit :: MonadUnliftIO m => Day -> Integer -> Acquire (ConduitT () a m ()) -> (a -> (Day, Day)) -> m (Map Day Word64)
makeUpcomingEventsMapWithLimit today limit source func =
  makeDayMap
    source
    ( \m a ->
        let (created, scheduled) = func a
         in addToUpcomingEventsMapWithLimit today limit m created scheduled
    )

addToUpcomingEventsMapWithLimit :: Day -> Integer -> Map Day Word64 -> Day -> Day -> Map Day Word64
addToUpcomingEventsMapWithLimit today limit m created scheduled =
  let days = [max created (addDays (-limit) scheduled) .. min today scheduled]
      mapOfOnes = M.fromAscList $ map (\d -> (d, 1)) days
   in M.unionWith (+) mapOfOnes m

makeDayMap :: MonadUnliftIO m => Acquire (ConduitT () a m ()) -> (Map Day Word64 -> a -> Map Day Word64) -> m (Map Day Word64)
makeDayMap acqSource func = withAcquire acqSource $ \source ->
  runConduit $ source .| C.foldl func M.empty

lineChart :: Day -> Day -> [(Text, Map Day Word64)] -> Widget
lineChart minDay maxDay maps = do
  let days = [minDay .. maxDay]
      labels = toJSON $ map (formatTime defaultTimeLocale "%F") days
      datumsFor dayCountMap = map (\d -> fromMaybe 0 $ M.lookup d dayCountMap) days
      datasetFor i (name, dayCountMap) =
        object
          [ "label" .= name,
            "data" .= toJSON (datumsFor dayCountMap),
            "fill" .= False,
            "borderColor" .= colours !! (i `mod` length colours)
          ]
      datasets = toJSON $ iterateL datasetFor maps
  classIdent <- newIdent
  $(widgetFile "admin/charts/line-chart")

barChart :: Day -> Day -> [(Text, Map Day Word64)] -> Widget
barChart minDay maxDay maps = do
  let days = [minDay .. maxDay]
      labels = toJSON $ map (formatTime defaultTimeLocale "%F") days
      datumsFor dayCountMap = map (\d -> fromMaybe 0 $ M.lookup d dayCountMap) days
      datasetFor i (name, dayCountMap) =
        object
          [ "label" .= name,
            "data" .= toJSON (datumsFor dayCountMap),
            "fill" .= False,
            "backgroundColor" .= colours !! (i `mod` length colours),
            "borderColor" .= colours !! (i `mod` length colours)
          ]
      datasets = toJSON $ iterateL datasetFor maps
  classIdent <- newIdent
  $(widgetFile "admin/charts/bar-chart")

iterateL :: (Int -> a -> b) -> [a] -> [b]
iterateL func = zipWith func [0 ..]

colours :: [Text]
colours =
  [ "rgb(75, 192, 192)",
    "rgb(54, 162, 235)",
    "rgb(153, 102, 255)",
    "rgb(201, 203, 207)"
  ]
