{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Sitemap where

import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as C
import Salsa.Party.Web.Server.Handler.Explore
import Salsa.Party.Web.Server.Handler.Import
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
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
          sitemapLastMod = Nothing,
          sitemapChangeFreq = Nothing,
          sitemapPriority = Just 0.8
        }
    forM_ locations $ \location ->
      yield
        SitemapUrl
          { sitemapLoc = SearchR location,
            sitemapLastMod = Nothing,
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
            { sitemapLoc = PartyR partyUuid,
              sitemapLastMod = Just $ fromMaybe partyCreated partyModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.4
            }
      )
    dbAcq
      acqImages
      ( \(Entity _ Image {..}) ->
          SitemapUrl
            { sitemapLoc = PosterR imageKey,
              sitemapLastMod = Just imageCreated,
              sitemapChangeFreq = Just Never,
              sitemapPriority = Just 0.3
            }
      )
    dbAcq
      acqExternalEvents
      ( \(Entity _ ExternalEvent {..}) ->
          SitemapUrl
            { sitemapLoc = PartyR externalEventUuid,
              sitemapLastMod = Just $ fromMaybe externalEventCreated externalEventModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.1
            }
      )

getRobotsR :: Handler Text
getRobotsR = robots SitemapR
