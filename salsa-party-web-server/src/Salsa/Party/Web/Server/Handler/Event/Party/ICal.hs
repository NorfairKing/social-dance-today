{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.ICal
  ( partyPageICal,
    partyCalendar,
    partyCalendarEvent,
  )
where

import qualified Data.Text as T
import qualified ICal.Component as ICal
import qualified ICal.Property as ICal
import qualified ICal.PropertyType.Date as ICal
import qualified ICal.PropertyType.DateTime as ICal
import qualified ICal.PropertyType.URI as ICal
import Network.URI
import Salsa.Party.Web.Server.Handler.Import

partyPageICal :: Entity Organiser -> Entity Party -> Handler ICal.Calendar
partyPageICal (Entity _ organiser) (Entity _ party@Party {..}) = do
  place <- runDB $ get404 partyPlace
  renderUrl <- getUrlRender
  pure $ partyCalendar renderUrl organiser party place

partyCalendar :: (Route App -> Text) -> Organiser -> Party -> Place -> ICal.Calendar
partyCalendar renderUrl organiser party place =
  (ICal.makeCalendar (ICal.ProdId (renderUrl HomeR)))
    { ICal.calendarEvents = [partyCalendarEvent renderUrl organiser party place]
    }

partyCalendarEvent :: (Route App -> Text) -> Organiser -> Party -> Place -> ICal.Event
partyCalendarEvent renderUrl organiser party@Party {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in ( ICal.makeEvent
          (ICal.UID (uuidText partyUuid))
          (ICal.DateTimeStamp (ICal.DateTimeUTC (fromMaybe partyCreated partyModified)))
      )
        { ICal.eventDateTimeStart = Just $ case partyStart of
            Nothing -> ICal.DateTimeStartDate (ICal.Date partyDay)
            Just start -> ICal.DateTimeStartDateTime (ICal.DateTimeFloating (LocalTime partyDay start)),
          ICal.eventCreated = Just $ ICal.Created partyCreated,
          ICal.eventDescription = ICal.Description <$> partyDescription,
          ICal.eventGeographicPosition =
            Just $
              ICal.GeographicPosition
                { ICal.geographicPositionLat = latitudeToFloat placeLat,
                  ICal.geographicPositionLon = longitudeToFloat placeLon
                },
          ICal.eventLastModified = ICal.LastModified <$> partyModified,
          ICal.eventLocation = Just $ ICal.Location placeQuery,
          ICal.eventStatus =
            Just $
              if partyCancelled
                then ICal.StatusCancelled
                else ICal.StatusConfirmed,
          ICal.eventSummary = Just $ ICal.Summary partyTitle,
          ICal.eventTransparency = ICal.TransparencyTransparent,
          ICal.eventURL = do
            -- We go through uri to make sure it's a valid uri.
            uri <- parseURI $ T.unpack $ renderUrl $ partyRoute organiser party
            pure $ ICal.URL $ ICal.URI uri
        }
