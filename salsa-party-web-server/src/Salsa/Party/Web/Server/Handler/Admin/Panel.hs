{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Panel where

import Salsa.Party.Looper.OrganiserReminder
import Salsa.Party.Web.Server.Handler.Import
import Text.Time.Pretty

getAdminPanelR :: Handler Html
getAdminPanelR = do
  now <- liftIO getCurrentTime
  let today = utctDay now

  nbUsers <- runDB $ count ([] :: [Filter User])
  nbOrganisers <- runDB $ count ([] :: [Filter Organiser])
  nbOrganiserReminders <- runDB $ count ([] :: [Filter OrganiserReminder])
  nbUpcomingParties <- runDB $ count ([PartyDay >=. today] :: [Filter Party])
  nbUpcomingParties7Days <- runDB $ count ([PartyDay >=. today, PartyDay <=. addDays 7 today] :: [Filter Party])
  nbUpcomingParties30Days <- runDB $ count ([PartyDay >=. today, PartyDay <=. addDays 30 today] :: [Filter Party])
  nbParties <- runDB $ count ([] :: [Filter Party])
  nbUpcomingExternalEvents <- runDB $ count ([ExternalEventDay >=. today] :: [Filter ExternalEvent])
  nbUpcomingExternalEvents7Days <- runDB $ count ([ExternalEventDay >=. today, ExternalEventDay <=. addDays 7 today] :: [Filter ExternalEvent])
  nbUpcomingExternalEvents30Days <- runDB $ count ([ExternalEventDay >=. today, ExternalEventDay <=. addDays 30 today] :: [Filter ExternalEvent])
  nbExternalEvents <- runDB $ count ([] :: [Filter ExternalEvent])
  nbSchedules <- runDB $ count ([] :: [Filter Schedule])
  nbProspects <- runDB $ count ([] :: [Filter Prospect])

  mLatestUser <- runDB $ selectFirst [] [Desc UserCreated]
  mLatestOrganiser <- runDB $ selectFirst [] [Desc OrganiserCreated]
  mLatestParty <- runDB $ selectFirst [] [Desc PartyCreated]
  mLatestPartySchedule <- runDB $ selectFirst [] [Desc ScheduleCreated]

  importers <- do
    importers <- runDB $ selectList [] [Asc ImporterMetadataId]
    forM importers $ \importer -> do
      upcomingEvents <- runDB $ count [ExternalEventImporter ==. entityKey importer, ExternalEventDay >=. today]
      events <- runDB $ count [ExternalEventImporter ==. entityKey importer]
      pure (importer, upcomingEvents, events)
  token <- genToken
  timeLocale <- getTimeLocale
  withNavBar $ do
    setTitle "Admin Panel"
    setDescriptionIdemp "Admin panel for the salsa parties admin"
    $(widgetFile "admin/panel")

getAdminUsersR :: Handler Html
getAdminUsersR = redirect $ AdminR $ AdminUsersPageR paginatedFirstPage

getAdminUsersPageR :: PageNumber -> Handler Html
getAdminUsersPageR pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize [] [Asc UserCreated, Asc UserId] pageNumber
  withNavBar $ do
    setTitle "Admin Users"
    setDescriptionIdemp "Admin overview of the users"
    $(widgetFile "admin/users")

getAdminUserR :: UserId -> Handler Html
getAdminUserR userId = do
  User {..} <- runDB $ get404 userId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  timeLocale <- getTimeLocale
  today <- liftIO $ utctDay <$> getCurrentTime
  token <- genToken
  (partiesCount, upcomingPartiesCount, mLastParty, mOrganiserReminder) <- case mOrganiser of
    Nothing -> pure (0, 0, Nothing, Nothing)
    Just (Entity organiserId _) ->
      runDB $
        (,,,)
          <$> count [PartyOrganiser ==. organiserId]
          <*> count [PartyOrganiser ==. organiserId, PartyDay >=. today]
          <*> selectFirst [PartyOrganiser ==. organiserId] [Desc PartyDay]
          <*> getBy (UniqueOrganiserReminderOrganiser organiserId)
  withNavBar $(widgetFile "admin/user")

postAdminUserImpersonateR :: UserId -> Handler Html
postAdminUserImpersonateR userId = do
  User {..} <- runDB $ get404 userId
  setCreds
    False
    Creds
      { credsPlugin = "impersonation",
        credsIdent = emailAddressText userEmailAddress,
        credsExtra = []
      }
  addMessage "is-success" $ "You are now impersonating the user with email address" <> toHtml (show userEmailAddress)
  redirect $ AccountR AccountOverviewR

getAdminOrganisersR :: Handler Html
getAdminOrganisersR = redirect $ AdminR $ AdminOrganisersPageR paginatedFirstPage

getAdminOrganisersPageR :: PageNumber -> Handler Html
getAdminOrganisersPageR pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize [] [Asc OrganiserId] pageNumber
  withNavBar $ do
    setTitle "Admin Organisers"
    setDescriptionIdemp "Admin overview of the organisers"
    $(widgetFile "admin/organisers")

getAdminOrganiserRemindersR :: Handler Html
getAdminOrganiserRemindersR = redirect $ AdminR $ AdminOrganiserRemindersPageR paginatedFirstPage

getAdminOrganiserRemindersPageR :: PageNumber -> Handler Html
getAdminOrganiserRemindersPageR pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize [] [Asc OrganiserReminderId] pageNumber
  reminders <- runDB $
    forM (paginatedElements paginated) $ \organiserReminderEntity@(Entity _ organiserReminder) -> do
      mOrganiser <- get $ organiserReminderOrganiser organiserReminder
      decision <- makeOrganiserReminderDecision organiserReminderEntity
      pure (organiserReminder, mOrganiser, decision)
  withNavBar $ do
    setTitle "Admin Organiser Reminders"
    setDescriptionIdemp "Admin overview of the organiser reminders"
    $(widgetFile "admin/organiser-reminders")

postAdminDeleteEventR :: EventUUID -> Handler Html
postAdminDeleteEventR uuid = do
  runDB $ deleteWhere [ExternalEventUuid ==. uuid]
  redirect $ AdminR AdminPanelR

getAdminPartiesR :: Handler Html
getAdminPartiesR = redirect $ AdminR $ AdminPartiesPageR paginatedFirstPage

getAdminPartiesPageR :: PageNumber -> Handler Html
getAdminPartiesPageR = adminPartiesPage [] [Asc PartyDay, Asc PartyId] (AdminR . AdminPartiesPageR)

getAdminUpcomingPartiesR :: Handler Html
getAdminUpcomingPartiesR = redirect $ AdminR $ AdminUpcomingPartiesPageR paginatedFirstPage

getAdminUpcomingPartiesPageR :: PageNumber -> Handler Html
getAdminUpcomingPartiesPageR pageNumber = do
  today <- liftIO $ utctDay <$> getCurrentTime
  adminPartiesPage [PartyDay >=. today] [Asc PartyDay, Asc PartyId] (AdminR . AdminUpcomingPartiesPageR) pageNumber

adminPartiesPage :: [Filter Party] -> [SelectOpt Party] -> (PageNumber -> Route App) -> PageNumber -> Handler Html
adminPartiesPage filters sorters pageRoute pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize filters sorters pageNumber
  withNavBar $ do
    setTitle "Admin Parties"
    setDescriptionIdemp "Admin overview of the parties"
    $(widgetFile "admin/parties")

getAdminExternalEventsR :: Handler Html
getAdminExternalEventsR = redirect $ AdminR $ AdminExternalEventsPageR paginatedFirstPage

getAdminExternalEventsPageR :: PageNumber -> Handler Html
getAdminExternalEventsPageR =
  externalEventsListPage
    []
    [Desc ExternalEventModified, Desc ExternalEventCreated]
    (AdminR . AdminExternalEventsPageR)

getAdminUpcomingExternalEventsR :: Handler Html
getAdminUpcomingExternalEventsR = redirect $ AdminR $ AdminUpcomingExternalEventsPageR paginatedFirstPage

getAdminUpcomingExternalEventsPageR :: PageNumber -> Handler Html
getAdminUpcomingExternalEventsPageR pn = do
  today <- liftIO $ utctDay <$> getCurrentTime
  externalEventsListPage
    [ExternalEventDay >=. today]
    [Desc ExternalEventModified, Desc ExternalEventCreated]
    (AdminR . AdminUpcomingExternalEventsPageR)
    pn

getAdminImporterEventsR :: ImporterMetadataId -> Handler Html
getAdminImporterEventsR importerId = redirect $ AdminR $ AdminImporterEventsPageR importerId paginatedFirstPage

getAdminImporterEventsPageR :: ImporterMetadataId -> PageNumber -> Handler Html
getAdminImporterEventsPageR importerId =
  externalEventsListPage
    [ExternalEventImporter ==. importerId]
    [Desc ExternalEventModified, Desc ExternalEventCreated]
    (AdminR . AdminImporterEventsPageR importerId)

getAdminImporterUpcomingEventsR :: ImporterMetadataId -> Handler Html
getAdminImporterUpcomingEventsR importerId = redirect $ AdminR $ AdminImporterUpcomingEventsPageR importerId paginatedFirstPage

getAdminImporterUpcomingEventsPageR :: ImporterMetadataId -> PageNumber -> Handler Html
getAdminImporterUpcomingEventsPageR importerId pn = do
  today <- liftIO $ utctDay <$> getCurrentTime
  externalEventsListPage
    [ ExternalEventImporter ==. importerId,
      ExternalEventDay >=. today
    ]
    [Desc ExternalEventModified, Desc ExternalEventCreated]
    (AdminR . AdminImporterUpcomingEventsPageR importerId)
    pn

externalEventsListPage :: [Filter ExternalEvent] -> [SelectOpt ExternalEvent] -> (PageNumber -> Route App) -> PageNumber -> Handler Html
externalEventsListPage filters sorters pageRoute pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize filters sorters pageNumber
  token <- genToken
  withNavBar $ do
    setTitle "Admin External Events"
    setDescriptionIdemp "Admin overview of the external events"
    $(widgetFile "admin/external-events")

getAdminSchedulesR :: Handler Html
getAdminSchedulesR = redirect $ AdminR $ AdminSchedulesPageR paginatedFirstPage

getAdminSchedulesPageR :: PageNumber -> Handler Html
getAdminSchedulesPageR = adminSchedulesPage [] [Asc ScheduleId] (AdminR . AdminSchedulesPageR)

adminSchedulesPage :: [Filter Schedule] -> [SelectOpt Schedule] -> (PageNumber -> Route App) -> PageNumber -> Handler Html
adminSchedulesPage filters sorters pageRoute pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize filters sorters pageNumber
  withNavBar $ do
    setTitle "Admin Schedules"
    setDescriptionIdemp "Admin overview of the schedules"
    $(widgetFile "admin/schedules")

getAdminProspectsR :: Handler Html
getAdminProspectsR = redirect $ AdminR $ AdminProspectsPageR paginatedFirstPage

getAdminProspectsPageR :: PageNumber -> Handler Html
getAdminProspectsPageR = adminProspectsPage [] [Asc ProspectInvited, Desc ProspectCreated] (AdminR . AdminProspectsPageR)

adminProspectsPage :: [Filter Prospect] -> [SelectOpt Prospect] -> (PageNumber -> Route App) -> PageNumber -> Handler Html
adminProspectsPage filters sorters pageRoute pageNumber = do
  paginated <- runDB $ selectPaginated defaultPageSize filters sorters pageNumber
  withNavBar $ do
    setTitle "Admin Prospects"
    setDescriptionIdemp "Admin overview of the prospects"
    $(widgetFile "admin/prospects")

postAdminImporterResetR :: ImporterMetadataId -> Handler Html
postAdminImporterResetR importerId = do
  runDB $
    update
      importerId
      [ ImporterMetadataLastRunStart =. Nothing,
        ImporterMetadataLastRunEnd =. Nothing,
        ImporterMetadataLastRunImported =. Just 0
      ]
  redirect $ AdminR AdminPanelR

postAdminImporterDeleteR :: ImporterMetadataId -> Handler Html
postAdminImporterDeleteR importerId = do
  today <- liftIO $ utctDay <$> getCurrentTime
  runDB $ do
    deleteWhere [ExternalEventImporter ==. importerId, ExternalEventDay >=. today]
    update
      importerId
      [ ImporterMetadataLastRunStart =. Nothing,
        ImporterMetadataLastRunEnd =. Nothing,
        ImporterMetadataLastRunImported =. Just 0
      ]
  redirect $ AdminR AdminPanelR

data Paginated a = Paginated
  { paginatedCurrentPage :: !PageNumber,
    paginatedTotalPages :: !PageNumber,
    paginatedTotalElements :: !Int,
    paginatedElements :: ![a],
    paginatedPreviousPage :: Maybe PageNumber,
    paginatedNextPage :: Maybe PageNumber,
    paginatedLastPage :: PageNumber
  }
  deriving (Show, Eq)

paginatedFirstPage :: PageNumber
paginatedFirstPage = 1

selectPaginated ::
  (PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
  Int ->
  [Filter a] ->
  [SelectOpt a] ->
  PageNumber ->
  SqlPersistT Handler (Paginated (Entity a))
selectPaginated pageSize filters opts paginatedCurrentPage = do
  paginatedTotalElements <- count filters
  let paginatedTotalPages = max paginatedFirstPage $ ceiling (fromIntegral paginatedTotalElements / (fromIntegral pageSize :: Double))
  paginatedElements <- selectList filters $ OffsetBy ((paginatedCurrentPage - 1) * pageSize) : LimitTo pageSize : opts
  let paginatedPreviousPage = if paginatedCurrentPage <= paginatedFirstPage then Nothing else Just $ pred paginatedCurrentPage
  let paginatedLastPage = paginatedTotalPages
  let paginatedNextPage = if paginatedCurrentPage >= paginatedLastPage then Nothing else Just $ succ paginatedCurrentPage
  pure Paginated {..}

paginationWidget :: (PageNumber -> Route App) -> Paginated a -> PageNumber -> Widget
paginationWidget page Paginated {..} pageNumber =
  let shouldShowFirst = pageNumber /= paginatedFirstPage && Just paginatedFirstPage /= paginatedPreviousPage
      shouldShowLast = pageNumber /= paginatedLastPage && Just paginatedLastPage /= paginatedNextPage
   in $(widgetFile "pagination")

defaultPageSize :: Int
defaultPageSize = 50

formatAdminDay :: Day -> Widget
formatAdminDay day = do
  timeLocale <- getTimeLocale
  today <- liftIO $ utctDay <$> getCurrentTime
  [whamlet|
    #{formatTime timeLocale "%F" day}
    (#{prettyDayAuto today day})
  |]

formatAdminTime :: UTCTime -> Widget
formatAdminTime time = do
  timeLocale <- getTimeLocale
  now <- liftIO getCurrentTime
  [whamlet|
    #{formatTime timeLocale "%F %H:%M" time}
    (#{prettyTimeAuto now time})
  |]
