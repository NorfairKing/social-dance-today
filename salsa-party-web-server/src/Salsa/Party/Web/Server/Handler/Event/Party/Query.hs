{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Event.Party.Query
  ( getPartyTupBySlug,
    getPartyTripBySlug,
    getPartyTupByUuid,
  )
where

import Control.Monad.IO.Class
import Data.Time
import Database.Esqueleto.Experimental
import Salsa.Party.DB

getPartyTupBySlug :: MonadIO m => OrganiserSlug -> EventSlug -> Day -> SqlPersistT m (Maybe (Organiser, Entity Party))
getPartyTupBySlug organiserSlug_ partySlug_ day =
  fmap (fmap (\(Entity _ organiser, partyEntity) -> (organiser, partyEntity))) $
    selectOne $ do
      (organiser :& party) <-
        from $
          table @Organiser
            `innerJoin` table @Party
            `on` ( \(organiser :& party) ->
                     organiser ^. OrganiserId
                       ==. party ^. PartyOrganiser
                 )
      where_ $ party ^. PartyDay ==. val day
      where_ $ organiser ^. OrganiserSlug ==. just (val organiserSlug_)
      where_ $ party ^. PartySlug ==. just (val partySlug_)
      pure (organiser, party)

getPartyTripBySlug :: MonadIO m => OrganiserSlug -> EventSlug -> Day -> SqlPersistT m (Maybe (Organiser, Party, Maybe Recurrence))
getPartyTripBySlug organiserSlug_ partySlug_ day =
  fmap (fmap (\(Entity _ organiser, Entity _ partyEntity, Value s) -> (organiser, partyEntity, s))) $
    selectOne $ do
      ((organiser :& party) :& (_ :& schedule)) <-
        from $
          ( table @Organiser
              `innerJoin` table @Party
              `on` ( \(organiser :& party) ->
                       organiser ^. OrganiserId
                         ==. party ^. PartyOrganiser
                   )
          )
            `leftJoin` ( table @ScheduleParty
                           `innerJoin` table @Schedule
                           `on` ( \(scheduleParty :& schedule) ->
                                    scheduleParty ^. SchedulePartySchedule ==. schedule ^. ScheduleId
                                )
                       )
            `on` ( \((_ :& party) :& (scheduleParty :& _)) ->
                     just (party ^. PartyId) ==. scheduleParty ?. SchedulePartyParty
                 )
      where_ $ party ^. PartyDay ==. val day
      where_ $ organiser ^. OrganiserSlug ==. just (val organiserSlug_)
      where_ $ party ^. PartySlug ==. just (val partySlug_)
      pure (organiser, party, schedule ?. ScheduleRecurrence)

getPartyTupByUuid :: MonadIO m => EventUUID -> SqlPersistT m (Maybe (Organiser, Entity Party))
getPartyTupByUuid partyUuid_ =
  fmap (fmap (\(Entity _ organiser, partyEntity) -> (organiser, partyEntity))) $
    selectOne $ do
      (organiser :& party) <-
        from $
          table @Organiser
            `innerJoin` table @Party
            `on` ( \(organiser :& party) ->
                     organiser ^. OrganiserId
                       ==. party ^. PartyOrganiser
                 )

      where_ $ party ^. PartyUuid ==. val partyUuid_
      pure (organiser, party)
