{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Sitemap where

import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Text.Shakespeare.Text
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = addDays (-1) today
  acqOrganisers <- runDB $ selectSourceRes [] [Asc OrganiserId]
  acqParties <- runDB $ selectSourceRes [] [Asc PartyId]
  acqImages <- runDB $ selectSourceRes [] [Asc ImageId]
  acqExternalEvents <- runDB $ selectSourceRes [] [Asc ExternalEventId]

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
      ( \(Entity _ Organiser {..}) ->
          SitemapUrl
            { sitemapLoc = OrganiserR organiserUuid,
              sitemapLastMod = Just $ fromMaybe organiserCreated organiserModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.5
            }
      )
    dbAcq
      acqParties
      ( \(Entity _ Party {..}) ->
          SitemapUrl
            { sitemapLoc = EventR partyUuid,
              sitemapLastMod = Just $ fromMaybe partyCreated partyModified,
              sitemapChangeFreq = if partyDay >= yesterday then Nothing else Just Never,
              sitemapPriority = Just $ if partyDay >= yesterday then 0.4 else 0.2
            }
      )
    dbAcq
      acqExternalEvents
      ( \(Entity _ ExternalEvent {..}) ->
          SitemapUrl
            { sitemapLoc = EventR externalEventUuid,
              sitemapLastMod = Just $ fromMaybe externalEventCreated externalEventModified,
              sitemapChangeFreq = if externalEventDay >= yesterday then Nothing else Just Never,
              sitemapPriority = Just $ if externalEventDay >= yesterday then 0.3 else 0.1
            }
      )
    dbAcq
      acqImages
      ( \(Entity _ Image {..}) ->
          SitemapUrl
            { sitemapLoc = ImageR imageKey,
              sitemapLastMod = Just imageCreated,
              sitemapChangeFreq = Just Never,
              sitemapPriority = Just 0.1
            }
      )

getRobotsR :: Handler TL.Text
getRobotsR = do
  urlRender <- getUrlRenderParams
  pure $ TLB.toLazyText $ $(textFile "templates/robots.txt") urlRender
