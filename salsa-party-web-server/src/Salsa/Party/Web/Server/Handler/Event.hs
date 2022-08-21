{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Event
  ( getEventR,
    getEventIcsR,
    getEventExportR,
    getPartySlugR,
    getExternalEventSlugR,
  )
where

import Network.HTTP.Types
import Salsa.Party.Web.Server.Handler.Event.Export
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
import Salsa.Party.Web.Server.Handler.Event.ICal
import Salsa.Party.Web.Server.Handler.Event.Party
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import

getEventR :: EventUUID -> Handler TypedContent
getEventR eventUuid = do
  mPartyTup <- runDB $ getPartyTupByUuid eventUuid
  case mPartyTup of
    Just partyTup@(Entity _ organiser, Entity _ party) -> case partySlugRoute organiser party of
      Nothing -> partyPage partyTup
      Just route -> redirect route
    Nothing -> do
      mExternalEventTup <- runDB $ getExternalEventTupByUuid eventUuid
      case mExternalEventTup of
        Just (externalEventEntity@(Entity _ externalEvent), placeEntity, mCASKey) -> case externalEventSlugRoute externalEvent of
          Nothing -> externalEventPage externalEventEntity placeEntity mCASKey
          Just route -> redirect route
        -- If the UUID represents a party that does not exist, we will assume
        -- that the event has dissappeared from our database.
        -- This assumption makes sense because we can assume that the user got
        -- the UUID somewhere.
        -- If the user got the UUID somewhere then that source likely tracks
        -- back to our website somehow, which means the party existed at some
        -- point and it is gone instead of notFound.
        -- This follows directly from the causality property of UUIDs:
        -- If a uuid shows up in two different places, they are extremely
        -- likely to somehow be causally linked.
        Nothing -> gone

getPartySlugR :: OrganiserSlug -> EventSlug -> Day -> Handler TypedContent
getPartySlugR organiserSlug_ partySlug_ day = do
  mPartyTup <- runDB $ getPartyTupBySlug organiserSlug_ partySlug_ day
  case mPartyTup of
    Nothing -> goneOrNotFound day
    Just partyTup -> partyPage partyTup

getExternalEventSlugR :: EventSlug -> Day -> Handler TypedContent
getExternalEventSlugR externalEventSlug_ day = do
  mExternalEventTup <- runDB $ getExternalEventTupBySlug externalEventSlug_ day
  case mExternalEventTup of
    Nothing -> goneOrNotFound day
    Just (externalEventEntity, placeEntity, mCASKey) -> externalEventPage externalEventEntity placeEntity mCASKey

-- In cases where we can tell which day the party was supposed to be on, we'll
-- still serve up a 404 if the day is in the future.
goneOrNotFound :: Day -> Handler TypedContent
goneOrNotFound d = do
  today <- liftIO $ utctDay <$> getCurrentTime
  if d < today
    then -- If the day is in the past then either:
    -- There was a party, in which case it's definitely not coming back because it's in the past.
    -- There was no party, in which case there also won't be any.
      gone
    else notFound

-- We serve up a 410 just in case search engines treat those differently and
-- correctly remove the page from their index.
gone :: Handler TypedContent
gone = do
  html <- withNavBar $ do
    setTitleI MsgGone
    $(widgetFile "error/410")
  sendResponseStatus gone410 html
