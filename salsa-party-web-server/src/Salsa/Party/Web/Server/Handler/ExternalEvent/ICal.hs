{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.ExternalEvent.ICal
  ( externalEventCalendar,
    externalEventCalendarEvent,
  )
where

import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.URI
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.ICalendar as ICal

externalEventCalendar :: (Route App -> Text) -> ExternalEvent -> Place -> ICal.VCalendar
externalEventCalendar renderUrl externalEvent@ExternalEvent {..} place =
  def
    { ICal.vcEvents =
        M.singleton
          (LT.fromStrict $ uuidText externalEventUuid, Just dateTime)
          (externalEventCalendarEvent renderUrl externalEvent place)
    }
  where
    dateTime = case externalEventStart of
      Nothing -> Left $ ICal.Date externalEventDay
      Just start -> Right $ ICal.FloatingDateTime (LocalTime externalEventDay start)

externalEventCalendarEvent :: (Route App -> Text) -> ExternalEvent -> Place -> ICal.VEvent
externalEventCalendarEvent renderUrl ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      noOther = def
   in ICal.VEvent
        { ICal.veDTStamp =
            ICal.DTStamp
              { ICal.dtStampValue =
                  fromMaybe externalEventCreated externalEventModified,
                ICal.dtStampOther = noOther
              },
          ICal.veUID =
            ICal.UID
              { ICal.uidValue = LT.fromStrict $ uuidText externalEventUuid,
                ICal.uidOther = noOther
              },
          ICal.veClass =
            ICal.Class
              { ICal.classValue = ICal.Public,
                ICal.classOther = noOther
              },
          ICal.veDTStart = Just $ case externalEventStart of
            Nothing ->
              ICal.DTStartDate
                { ICal.dtStartDateValue = ICal.Date externalEventDay,
                  dtStartOther = noOther
                }
            Just start ->
              ICal.DTStartDateTime
                { ICal.dtStartDateTimeValue = ICal.FloatingDateTime (LocalTime externalEventDay start),
                  ICal.dtStartOther = noOther
                },
          ICal.veCreated =
            Just $
              ICal.Created
                { ICal.createdValue = externalEventCreated,
                  ICal.createdOther = noOther
                },
          ICal.veDescription =
            ( \description ->
                ICal.Description
                  { ICal.descriptionValue = LT.fromStrict description,
                    ICal.descriptionAltRep = Nothing,
                    ICal.descriptionLanguage = Nothing,
                    ICal.descriptionOther = noOther
                  }
            )
              <$> externalEventDescription,
          ICal.veGeo =
            Just $
              ICal.Geo
                { ICal.geoLat = realToFrac placeLat,
                  ICal.geoLong = realToFrac placeLon,
                  ICal.geoOther = noOther
                },
          ICal.veLastMod =
            ( \modified ->
                ICal.LastModified
                  { ICal.lastModifiedValue = modified,
                    ICal.lastModifiedOther = noOther
                  }
            )
              <$> externalEventModified,
          ICal.veLocation =
            Just $
              ICal.Location
                { ICal.locationValue = LT.fromStrict placeQuery,
                  ICal.locationAltRep = Nothing,
                  ICal.locationLanguage = Nothing,
                  ICal.locationOther = noOther
                },
          ICal.veOrganizer = Nothing,
          ICal.vePriority = def,
          ICal.veSeq = def,
          ICal.veStatus =
            Just $
              if externalEventCancelled
                then ICal.CancelledEvent {eventStatusOther = noOther}
                else ICal.ConfirmedEvent {eventStatusOther = noOther},
          ICal.veSummary =
            Just $
              ICal.Summary
                { ICal.summaryValue = LT.fromStrict externalEventTitle,
                  ICal.summaryAltRep = Nothing,
                  ICal.summaryLanguage = Nothing,
                  ICal.summaryOther = noOther
                },
          ICal.veTransp = ICal.Transparent {timeTransparencyOther = noOther},
          ICal.veUrl = do
            uri <- parseURI $ T.unpack $ renderUrl $ EventR externalEventUuid
            pure $ ICal.URL {ICal.urlValue = uri, ICal.urlOther = noOther},
          ICal.veRecurId = Nothing,
          ICal.veRRule = S.empty,
          ICal.veDTEndDuration = Nothing,
          ICal.veAttach = S.empty,
          ICal.veAttendee = S.empty,
          ICal.veCategories = S.empty,
          ICal.veComment = S.empty,
          ICal.veContact = S.empty,
          ICal.veExDate = S.empty,
          ICal.veRStatus = S.empty,
          ICal.veRelated = S.empty,
          ICal.veResources = S.empty,
          ICal.veRDate = S.empty,
          ICal.veAlarms = S.empty,
          ICal.veOther = S.empty
        }
