{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Salsa.Party.Web.Server.Handler.Import
import Text.Julius

getAdminChartsR :: Handler Html
getAdminChartsR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  acqExternalEventsSource <- runDB $ selectSourceRes [] [Asc ExternalEventId]
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

  -- Users and organisers
  acqUsersSource <- runDB $ selectSourceRes [] [Asc UserId]
  dayCountOfUsers <- makeDayCountMap today acqUsersSource $ utctDay . userCreated . entityVal
  acqOrganisersSource <- runDB $ selectSourceRes [] [Asc OrganiserId]
  dayCountOfOrganisers <- makeDayCountMap today acqOrganisersSource $ utctDay . organiserCreated . entityVal

  -- Per day
  partiesPerDayMap <- makePerDayCountMap acqPartiesSource $ partyDay . entityVal
  externalEventsPerDayMap <- makePerDayCountMap acqExternalEventsSource $ externalEventDay . entityVal

  let minDay = minimum $ map fst $ mapMaybe M.lookupMin [dayCountMapOfExternalEvents, dayCountMapOfParties]
      curDay = addDays (-1) today
      maxDay = addDays 30 today

  withNavBar $ do
    addScriptRemote "https://cdn.jsdelivr.net/npm/chart.js@3.4.1/dist/chart.min.js"
    $(widgetFile "admin/charts")

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
  let days = [max created (addDays (- limit) scheduled) .. min today scheduled]
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
