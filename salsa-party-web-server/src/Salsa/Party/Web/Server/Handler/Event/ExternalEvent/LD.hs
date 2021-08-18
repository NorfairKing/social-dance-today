{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
  ( externalEventPageLD,
    externalEventToLDEvent,
  )
where

import Salsa.Party.Web.Server.Handler.Import
import qualified Web.JSONLD as LD

externalEventPageLD :: Entity ExternalEvent -> Handler JSONLDData
externalEventPageLD (Entity externalEventId externalEvent@ExternalEvent {..}) = do
  place@Place {..} <- runDB $ get404 externalEventPlace
  mPosterKey <- runDB $ getPosterForExternalEvent externalEventId
  renderUrl <- getUrlRender
  pure $ toJSONLDData $ externalEventToLDEvent renderUrl externalEvent place mPosterKey

externalEventToLDEvent :: (Route App -> Text) -> ExternalEvent -> Place -> Maybe CASKey -> LD.Event
externalEventToLDEvent renderUrl ExternalEvent {..} Place {..} mPosterKey =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in LD.Event
        { LD.eventName = externalEventTitle,
          LD.eventLocation =
            LD.EventLocationPlace $
              LD.Place
                { LD.placeName = Nothing,
                  LD.placeAddress = LD.PlaceAddressText placeQuery,
                  LD.placeGeo =
                    Just $
                      LD.PlaceGeoCoordinates
                        LD.GeoCoordinates
                          { LD.geoCoordinatesLatitude = placeLat,
                            LD.geoCoordinatesLongitude = placeLon
                          }
                },
          LD.eventStartDate = case externalEventStart of
            Nothing -> LD.EventStartDate externalEventDay
            Just timeOfDay ->
              LD.EventStartDateTime
                LD.DateTime
                  { dateTimeLocalTime =
                      LocalTime
                        { localDay = externalEventDay,
                          localTimeOfDay = timeOfDay
                        },
                    dateTimeTimeZone = Nothing
                  },
          LD.eventDescription = externalEventDescription,
          LD.eventUrl = Nothing,
          LD.eventEndDate = Nothing,
          LD.eventAttendanceMode = Just LD.OfflineEventAttendanceMode,
          LD.eventStatus =
            Just $
              if externalEventCancelled
                then LD.EventCancelled
                else LD.EventScheduled,
          LD.eventImages = [LD.EventImageURL (renderUrl (ImageR posterKey)) | posterKey <- maybeToList mPosterKey],
          LD.eventOrganizer = case externalEventOrganiser of
            Nothing -> Nothing
            Just name ->
              Just $
                LD.EventOrganizerOrganization
                  LD.Organization
                    { LD.organizationName = name,
                      organizationUrl = Nothing
                    }
        }
