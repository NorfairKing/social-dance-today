{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
  ( externalEventPageICal,
    externalEventCalendar,
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

externalEventPageICal :: ExternalEvent -> Place -> Handler ICal.Calendar
externalEventPageICal externalEvent place = do
  renderUrl <- getUrlRender
  pure $ externalEventCalendar renderUrl externalEvent place

externalEventCalendar :: (Route App -> Text) -> ExternalEvent -> Place -> ICal.Calendar
externalEventCalendar renderUrl externalEvent place =
  (ICal.makeCalendar (ICal.ProdId (renderUrl HomeR)))
    { ICal.calendarEvents =
        [ externalEventCalendarEvent renderUrl externalEvent place
        ]
    }

externalEventCalendarEvent :: (Route App -> Text) -> ExternalEvent -> Place -> ICal.Event
externalEventCalendarEvent renderUrl externalEvent@ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in ( ICal.makeEvent
          (ICal.UID (uuidText externalEventUuid))
          (ICal.DateTimeStamp (ICal.DateTimeUTC (fromMaybe externalEventCreated externalEventModified)))
      )
        { ICal.eventDateTimeStart = Just $ case externalEventStart of
            Nothing -> ICal.DateTimeStartDate (ICal.Date externalEventDay)
            Just start -> ICal.DateTimeStartDateTime (ICal.DateTimeFloating (LocalTime externalEventDay start)),
          ICal.eventCreated = Just $ ICal.Created externalEventCreated,
          ICal.eventDescription = ICal.Description <$> externalEventDescription,
          ICal.eventGeographicPosition =
            Just $
              ICal.GeographicPosition
                { ICal.geographicPositionLat = latitudeToFloat placeLat,
                  ICal.geographicPositionLon = longitudeToFloat placeLon
                },
          ICal.eventLastModified = ICal.LastModified <$> externalEventModified,
          ICal.eventLocation = Just $ ICal.Location placeQuery,
          ICal.eventStatus =
            ( \c ->
                if c
                  then ICal.StatusCancelled
                  else ICal.StatusConfirmed
            )
              <$> externalEventCancelled,
          ICal.eventSummary = Just $ ICal.Summary externalEventTitle,
          ICal.eventTransparency = ICal.TransparencyTransparent,
          ICal.eventURL = do
            uri <- parseURI $ T.unpack $ renderUrl $ externalEventRoute externalEvent
            pure $ ICal.URL $ ICal.URI uri
        }
