{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.LD
  ( partyPageLD,
    partyToLDEvent,
  )
where

import Salsa.Party.Web.Server.Handler.Import
import qualified Web.JSONLD as LD

partyPageLD :: Organiser -> Party -> Handler JSONLDData
partyPageLD organiser party@Party {..} = do
  place <- runDB $ get404 partyPlace
  renderUrl <- getUrlRender
  pure $ toJSONLDData $ partyToLDEvent renderUrl party organiser place

partyToLDEvent :: (Route App -> Text) -> Party -> Organiser -> Place -> LD.Event
partyToLDEvent renderUrl Party {..} Organiser {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in LD.Event
        { LD.eventName = partyTitle,
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
          LD.eventStartDate = case partyStart of
            Nothing -> LD.EventStartDate (LD.Date partyDay)
            Just timeOfDay ->
              LD.EventStartDateTime
                LD.DateTime
                  { dateTimeLocalTime =
                      LocalTime
                        { localDay = partyDay,
                          localTimeOfDay = timeOfDay
                        },
                    dateTimeTimeZone = Nothing
                  },
          LD.eventDescription = partyDescription,
          LD.eventUrl = Nothing,
          LD.eventEndDate = Nothing,
          LD.eventAttendanceMode = Just LD.OfflineEventAttendanceMode,
          LD.eventStatus =
            Just $
              if partyCancelled
                then LD.EventCancelled
                else LD.EventScheduled,
          LD.eventImages = [LD.EventImageURL (renderUrl (ImageR posterKey)) | posterKey <- maybeToList partyPoster],
          LD.eventOrganizer =
            Just $
              LD.EventOrganizerOrganization
                LD.Organization
                  { LD.organizationName = organiserName,
                    organizationUrl = Just $ renderUrl (OrganiserR organiserUuid),
                    organizationLogo = Nothing,
                    organizationFounder = Nothing
                  }
        }
