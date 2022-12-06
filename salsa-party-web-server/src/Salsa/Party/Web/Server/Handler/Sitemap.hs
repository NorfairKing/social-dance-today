{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.Web.Server.Handler.Sitemap
  ( getSitemapR,
    getSitemapTopLevelR,
    getSitemapLocationsR,
    getSitemapOrganisersR,
    getSitemapEventsR,
    getRobotsR,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Int
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Database.Esqueleto.Legacy as E
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Text.Shakespeare.Text
import Web.SitemapIndex
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR =
  sitemapIndexList $
    concat
      [ [ SitemapIndexUrl
            { sitemapIndexLoc = SitemapTopLevelR,
              sitemapIndexLastMod = Nothing
            },
          SitemapIndexUrl
            { sitemapIndexLoc = SitemapLocationsR,
              -- We could provide a more accurate last modification but it would
              -- get in the way of responding quickly
              sitemapIndexLastMod = Nothing
            },
          SitemapIndexUrl
            { sitemapIndexLoc = SitemapOrganisersR,
              -- We could provide a more accurate last modification but it would
              -- get in the way of responding quickly
              sitemapIndexLastMod = Nothing
            }
        ],
        let minB = -180
            maxB = 180
            interval = 5
         in [ SitemapIndexUrl
                { sitemapIndexLoc = SitemapEventsR lo (lo + interval),
                  sitemapIndexLastMod = Nothing
                }
              | lo <- [minB, (minB + interval) .. (maxB - 1)]
            ]
      ]

getSitemapTopLevelR :: Handler TypedContent
getSitemapTopLevelR = do
  sitemapList
    [ SitemapUrl
        { sitemapLoc = HomeR,
          sitemapLastMod = Nothing,
          sitemapChangeFreq = Nothing,
          sitemapPriority = Just 0.9
        },
      SitemapUrl
        { sitemapLoc = ExploreR,
          sitemapLastMod = Nothing,
          sitemapChangeFreq = Just Hourly,
          sitemapPriority = Just 0.8
        }
    ]

getSitemapOrganisersR :: Handler TypedContent
getSitemapOrganisersR = do
  acqOrganisers <- runDB $ selectSourceRes [] [Asc OrganiserId]

  sitemap $ do
    dbAcq
      acqOrganisers
      ( \(Entity _ organiser@Organiser {..}) ->
          SitemapUrl
            { sitemapLoc = organiserRoute organiser,
              sitemapLastMod = Just $ fromMaybe organiserCreated organiserModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.4
            }
      )

getSitemapLocationsR :: Handler TypedContent
getSitemapLocationsR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  sitemap $ do
    forM_ locations $ \location ->
      yield
        SitemapUrl
          { sitemapLoc = SearchR $ placeQuery $ locationPlace location,
            sitemapLastMod = Just $ UTCTime today 0, -- At the beginning of the day
            sitemapChangeFreq = Just Daily,
            sitemapPriority = Just 0.6
          }

    forM_ locations $ \location -> do
      forM_ allDanceStyles $ \danceStyle ->
        yield
          SitemapUrl
            { sitemapLoc = SearchDanceStyleR (placeQuery $ locationPlace location) danceStyle,
              sitemapLastMod = Just $ UTCTime today 0, -- At the beginning of the day
              sitemapChangeFreq = Just Daily,
              sitemapPriority = Just 0.5
            }

getSitemapEventsR :: Int16 -> Int16 -> Handler TypedContent
getSitemapEventsR lo hi = do
  let loLon = Longitude $ fixedToCoord $ fromIntegral lo
  let hiLon = Longitude $ fixedToCoord $ fromIntegral hi
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = addDays (-1) today
  let earliestDayToShow = yesterday

  sitemap $ do
    C.transPipe
      runDB
      ( E.selectSource $
          E.from $ \(place `E.InnerJoin` party) -> do
            E.on $ place E.^. PlaceId E.==. party E.^. PartyPlace
            E.where_ $
              (place E.^. PlaceLon E.>=. E.val loLon)
                E.&&. (place E.^. PlaceLon E.<. E.val hiLon)
            E.where_ (party E.^. PartyDay E.>=. E.val earliestDayToShow)
            pure
              ( party E.^. PartyUuid,
                party E.^. PartyCreated,
                party E.^. PartyModified
              )
      )
      .| C.map
        ( \(E.Value uuid, E.Value created, E.Value modified) ->
            SitemapUrl
              { sitemapLoc =
                  -- We must use 'EventR' here instead of 'partyRoute' in case
                  -- the party title (and therefore slug) is changed inbetween
                  -- when the sitemap is requested and when the party is
                  -- requested.
                  -- This should not happen, but seems like it has already, in
                  -- the past.
                  -- Another good reason to do it this way is so that the
                  -- sitemap computation is faster.
                  EventR uuid,
                sitemapLastMod = Just $ fromMaybe created modified,
                sitemapChangeFreq = Nothing,
                sitemapPriority = Just 0.3
              }
        )
    C.transPipe
      runDB
      ( E.selectSource $
          E.from $ \(place `E.InnerJoin` externalEvent) -> do
            E.on $ place E.^. PlaceId E.==. externalEvent E.^. ExternalEventPlace
            E.where_ $
              (place E.^. PlaceLon E.>=. E.val loLon)
                E.&&. (place E.^. PlaceLon E.<. E.val hiLon)
            E.where_ (externalEvent E.^. ExternalEventDay E.>=. E.val earliestDayToShow)
            pure
              ( externalEvent E.^. ExternalEventUuid,
                externalEvent E.^. ExternalEventCreated,
                externalEvent E.^. ExternalEventModified
              )
      )
      .| C.map
        ( \(E.Value uuid, E.Value created, E.Value modified) ->
            SitemapUrl
              { sitemapLoc =
                  -- We must use 'EventR' here instead of 'externalEventRoute' in case
                  -- the external event title (and therefore slug) is changed inbetween
                  -- when the sitemap is requested and when the external event is
                  -- requested.
                  -- This should not happen, but seems like it has already, in
                  -- the past.
                  -- Another good reason to do it this way is so that the
                  -- sitemap computation is faster.
                  EventR uuid,
                sitemapLastMod = Just $ fromMaybe created modified,
                sitemapChangeFreq = Nothing,
                sitemapPriority = Just 0.2
              }
        )

getRobotsR :: Handler TL.Text
getRobotsR = do
  urlRender <- getUrlRenderParams
  pure $ TLB.toLazyText $ $(textFile "templates/robots.txt") urlRender

dbAcq ::
  Acquire (ConduitM () a Handler ()) ->
  (a -> SitemapUrl (Route App)) ->
  ConduitT () (SitemapUrl (Route App)) Handler ()
dbAcq acq func = do
  (rk, aSource) <- allocateAcquire acq
  aSource
    .| C.map func
  release rk
