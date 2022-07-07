module Salsa.Party.Web.Server.Handler.Organiser.ICal
  ( getOrganiserCalendarR,
    getOrganiserSlugCalendarR,
  )
where

import qualified Database.Esqueleto.Legacy as E
import qualified ICal.Component as ICal
import qualified ICal.Property as ICal
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Import

getOrganiserCalendarR :: OrganiserUUID -> Handler ICal.Calendar
getOrganiserCalendarR uuid = do
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUUID uuid
  case mOrganiser of
    Nothing -> notFound
    Just organiserEntity@(Entity _ organiser) -> case organiserSlug organiser of
      Just slug -> redirect $ OrganiserSlugCalendarR slug
      Nothing -> organiserCalendarPage organiserEntity

getOrganiserSlugCalendarR :: OrganiserSlug -> Handler ICal.Calendar
getOrganiserSlugCalendarR slug = do
  mOrganiser <- runDB $ selectFirst [OrganiserSlug ==. Just slug] []
  case mOrganiser of
    Nothing -> notFound
    Just organiserEntity -> organiserCalendarPage organiserEntity

organiserCalendarPage :: Entity Organiser -> Handler ICal.Calendar
organiserCalendarPage (Entity organiserId organiser) = do
  parties <- runDB $ getPartiesOfOrganiser organiserId
  renderUrl <- getUrlRender
  pure $
    (ICal.makeCalendar (ICal.ProdId (renderUrl HomeR)))
      { ICal.calendarEvents =
          map
            (\(Entity _ party, Entity _ place) -> partyCalendarEvent renderUrl organiser party place)
            parties
      }

getPartiesOfOrganiser :: MonadIO m => OrganiserId -> SqlPersistT m [(Entity Party, Entity Place)]
getPartiesOfOrganiser organiserId = do
  E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
      E.orderBy [E.asc $ party E.^. PartyDay]
      pure (party, p)
