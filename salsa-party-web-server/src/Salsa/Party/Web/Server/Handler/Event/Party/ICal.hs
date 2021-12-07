{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.ICal
  ( partyPageICal,
    partyCalendar,
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

partyPageICal :: Entity Organiser -> Entity Party -> Handler ICal.VCalendar
partyPageICal (Entity _ organiser) (Entity _ party@Party {..}) = do
  place <- runDB $ get404 partyPlace
  renderUrl <- getUrlRender
  pure $ partyCalendar renderUrl organiser party place

partyCalendar :: (Route App -> Text) -> Organiser -> Party -> Place -> ICal.VCalendar
partyCalendar renderUrl organiser party@Party {..} place =
  def
    { ICal.vcProdId =
        ICal.ProdId
          { ICal.prodIdValue = LT.fromStrict $ renderUrl HomeR,
            ICal.prodIdOther = def
          },
      ICal.vcEvents =
        M.singleton
          (LT.fromStrict $ uuidText partyUuid, Just dateTime)
          (partyCalendarEvent renderUrl organiser party place)
    }
  where
    dateTime = case partyStart of
      Nothing -> Left $ ICal.Date partyDay
      Just start -> Right $ ICal.FloatingDateTime (LocalTime partyDay start)

partyCalendarEvent :: (Route App -> Text) -> Organiser -> Party -> Place -> ICal.VEvent
partyCalendarEvent renderUrl organiser party@Party {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      noOther = def
   in ICal.VEvent
        { ICal.veDTStamp =
            ICal.DTStamp
              { ICal.dtStampValue =
                  fromMaybe partyCreated partyModified,
                ICal.dtStampOther = noOther
              },
          ICal.veUID =
            ICal.UID
              { ICal.uidValue = LT.fromStrict $ uuidText partyUuid,
                ICal.uidOther = noOther
              },
          ICal.veClass =
            ICal.Class
              { ICal.classValue = ICal.Public,
                ICal.classOther = noOther
              },
          ICal.veDTStart = Just $ case partyStart of
            Nothing ->
              ICal.DTStartDate
                { ICal.dtStartDateValue = ICal.Date partyDay,
                  dtStartOther = noOther
                }
            Just start ->
              ICal.DTStartDateTime
                { ICal.dtStartDateTimeValue = ICal.FloatingDateTime (LocalTime partyDay start),
                  ICal.dtStartOther = noOther
                },
          ICal.veCreated =
            Just $
              ICal.Created
                { ICal.createdValue = partyCreated,
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
              <$> partyDescription,
          ICal.veGeo =
            Just $
              ICal.Geo
                { ICal.geoLat = latitudeToFloat placeLat,
                  ICal.geoLong = longitudeToFloat placeLon,
                  ICal.geoOther = noOther
                },
          ICal.veLastMod =
            ( \modified ->
                ICal.LastModified
                  { ICal.lastModifiedValue = modified,
                    ICal.lastModifiedOther = noOther
                  }
            )
              <$> partyModified,
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
              if partyCancelled
                then ICal.CancelledEvent {eventStatusOther = noOther}
                else ICal.ConfirmedEvent {eventStatusOther = noOther},
          ICal.veSummary =
            Just $
              ICal.Summary
                { ICal.summaryValue = LT.fromStrict partyTitle,
                  ICal.summaryAltRep = Nothing,
                  ICal.summaryLanguage = Nothing,
                  ICal.summaryOther = noOther
                },
          ICal.veTransp = ICal.Transparent {timeTransparencyOther = noOther},
          ICal.veUrl = do
            -- We go through uri to make sure it's a valid uri.
            uri <- parseURI $ T.unpack $ renderUrl $ partyRoute organiser party
            pure $ ICal.URL {ICal.urlValue = LT.fromStrict $ T.pack $ show uri, ICal.urlOther = noOther},
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
