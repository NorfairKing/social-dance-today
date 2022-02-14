{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Organiser.ICal
  ( getOrganiserCalendarR,
    getOrganiserSlugCalendarR,
  )
where

import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto.Legacy as E
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar.Types as ICal

getOrganiserCalendarR :: OrganiserUUID -> Handler ICal.VCalendar
getOrganiserCalendarR uuid = do
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUUID uuid
  case mOrganiser of
    Nothing -> notFound
    Just organiserEntity@(Entity _ organiser) -> case organiserSlug organiser of
      Just slug -> redirect $ OrganiserSlugCalendarR slug
      Nothing -> organiserCalendarPage organiserEntity

getOrganiserSlugCalendarR :: OrganiserSlug -> Handler ICal.VCalendar
getOrganiserSlugCalendarR slug = do
  mOrganiser <- runDB $ selectFirst [OrganiserSlug ==. Just slug] []
  case mOrganiser of
    Nothing -> notFound
    Just organiserEntity -> organiserCalendarPage organiserEntity

organiserCalendarPage :: Entity Organiser -> Handler ICal.VCalendar
organiserCalendarPage (Entity organiserId organiser@Organiser {..}) = do
  undefined
