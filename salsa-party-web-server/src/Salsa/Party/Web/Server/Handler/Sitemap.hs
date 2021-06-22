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
  acqPosters <- runDB $ selectSourceRes [] [Asc PosterId]

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
    yield
      SitemapUrl
        { sitemapLoc = QueryR,
          sitemapLastMod = Nothing,
          sitemapChangeFreq = Nothing,
          sitemapPriority = Just 0.7
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
      ( \(Entity organiserId Organiser {..}) ->
          SitemapUrl
            { sitemapLoc = OrganiserR organiserId,
              sitemapLastMod = Just $ fromMaybe organiserCreated organiserModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.5
            }
      )
    dbAcq
      acqParties
      ( \(Entity partyId Party {..}) ->
          SitemapUrl
            { sitemapLoc = PartyR partyId,
              sitemapLastMod = Just $ fromMaybe partyCreated partyModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.4
            }
      )
    dbAcq
      acqPosters
      ( \(Entity _ Poster {..}) ->
          SitemapUrl
            { sitemapLoc = PosterR posterKey,
              sitemapLastMod = posterCreated <|> posterModified,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.2
            }
      )

getRobotsR :: Handler Text
getRobotsR = robots SitemapR
