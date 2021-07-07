{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Panel where

import Salsa.Party.Web.Server.Handler.Import

getAdminPanelR :: Handler Html
getAdminPanelR = do
  users <- runDB $ selectList [] [Asc UserId]
  organisers <- runDB $ selectList [] [Asc OrganiserId]
  token <- genToken
  withNavBar $ do
    setTitle "Salsa Parties Admin Panel"
    setDescription "Admin panel for the salsa parties admin"
    $(widgetFile "admin/panel")

postAdminUserImpersonateR :: UserId -> Handler Html
postAdminUserImpersonateR userId = do
  User {..} <- runDB $ get404 userId
  setCreds False Creds {credsPlugin = "impersonation", credsIdent = userEmailAddress, credsExtra = []}
  addMessage "is-success" $ "You are now impersonating user " <> toHtml userEmailAddress
  redirect $ AccountR AccountOverviewR

postAdminDeleteEventR :: EventUUID -> Handler Html
postAdminDeleteEventR uuid = do
  runDB $ deleteWhere [ExternalEventUuid ==. uuid]
  redirect $ AdminR AdminPanelR

getAdminPartiesR :: Handler Html
getAdminPartiesR = redirect $ AdminR $ AdminPartiesPageR paginatedFirstPage

getAdminPartiesPageR :: PageNumber -> Handler Html
getAdminPartiesPageR pageNumber = do
  paginated <- runDB $ selectPaginated 10 [] [Asc PartyId] pageNumber
  today <- liftIO $ utctDay <$> getCurrentTime
  withNavBar $ do
    setTitle "Salsa Parties Admin Parties"
    setDescription "Admin overview of the parties"
    $(widgetFile "admin/parties")

getAdminExternalEventsR :: Handler Html
getAdminExternalEventsR = redirect $ AdminR $ AdminExternalEventsPageR paginatedFirstPage

getAdminExternalEventsPageR :: PageNumber -> Handler Html
getAdminExternalEventsPageR pageNumber = do
  paginated <- runDB $ selectPaginated 10 [] [Asc ExternalEventId] pageNumber
  today <- liftIO $ utctDay <$> getCurrentTime
  token <- genToken
  withNavBar $ do
    setTitle "Salsa Parties Admin External Events"
    setDescription "Admin overview of the external events"
    $(widgetFile "admin/external-events")

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
  let paginatedTotalPages = ceiling $ fromIntegral paginatedTotalElements / (fromIntegral pageSize :: Double)
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
