{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.Web.Server.Handler.Sitemap where

import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Database.Esqueleto.Legacy as E
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search
import Text.Shakespeare.Text
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = addDays (-1) today
  let earliestDayToShow = yesterday
  let latestDayToShow = addDays maximumDaysAhead today
  acqOrganisers <- runDB $ selectSourceRes [] [Asc OrganiserId]
  acqExternalEvents <-
    runDB $
      selectSourceRes
        [ExternalEventDay >=. earliestDayToShow, ExternalEventDay <. latestDayToShow]
        [Asc ExternalEventId]

  sitemap $ do
    yield
      SitemapUrl
        { sitemapLoc = HomeR,
          sitemapLastMod = Nothing,
          sitemapChangeFreq = Nothing,
          sitemapPriority = Just 0.9
        }
    yield
      SitemapUrl
        { sitemapLoc = ExploreR,
          sitemapLastMod = Just $ UTCTime today 0, -- At the beginning of the day
          sitemapChangeFreq = Just Daily,
          sitemapPriority = Just 0.8
        }
    forM_ locations $ \location ->
      yield
        SitemapUrl
          { sitemapLoc = SearchR $ placeQuery $ locationPlace location,
            sitemapLastMod = Just $ UTCTime today 0, -- At the beginning of the day
            sitemapChangeFreq = Just Daily,
            sitemapPriority = Just 0.6
          }
    let dbAcq ::
          Acquire (ConduitM () a Handler ()) ->
          (a -> SitemapUrl (Route App)) ->
          ConduitT () (SitemapUrl (Route App)) Handler ()
        dbAcq acq func = do
          (rk, aSource) <- allocateAcquire acq
          aSource
            .| C.map func
          release rk
    dbAcq
      acqOrganisers
      ( \(Entity _ organiser@Organiser {..}) ->
          SitemapUrl
            { sitemapLoc = organiserRoute organiser,
              sitemapLastMod = Just $ fromMaybe organiserCreated organiserModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.5
            }
      )
    C.transPipe
      runDB
      ( E.selectSource $
          E.from $ \(organiser `E.InnerJoin` party) -> do
            E.on $ party E.^. PartyOrganiser E.==. organiser E.^. OrganiserId
            E.where_ $
              (party E.^. PartyDay E.>=. E.val earliestDayToShow)
                E.&&. (party E.^. PartyDay E.<. E.val earliestDayToShow)
            pure (organiser, party)
      )
      .| C.map
        ( \(Entity _ organiser, Entity _ party@Party {..}) ->
            SitemapUrl
              { sitemapLoc = partyRoute organiser party,
                sitemapLastMod = Just $ fromMaybe partyCreated partyModified,
                sitemapChangeFreq = Nothing,
                sitemapPriority = Just 0.4
              }
        )
    dbAcq
      acqExternalEvents
      ( \(Entity _ externalEvent@ExternalEvent {..}) ->
          SitemapUrl
            { sitemapLoc = externalEventRoute externalEvent,
              sitemapLastMod = Just $ fromMaybe externalEventCreated externalEventModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.3
            }
      )

getRobotsR :: Handler TL.Text
getRobotsR = do
  urlRender <- getUrlRenderParams
  pure $ TLB.toLazyText $ $(textFile "templates/robots.txt") urlRender
