{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Sitemap where

import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as C
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Handler.Explore
import Salsa.Party.Web.Server.Handler.Import
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  acqOrganiserIds <- runDB $ selectKeysRes [] [Asc OrganiserId]
  acqPartyIds <- runDB $ selectKeysRes [] [Asc PartyId]

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
      acqOrganiserIds
      ( \organiserId ->
          SitemapUrl
            { sitemapLoc = OrganiserR organiserId,
              sitemapLastMod = Nothing,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.5
            }
      )
    dbAcq
      acqPartyIds
      ( \partyId ->
          SitemapUrl
            { sitemapLoc = PartyR partyId,
              sitemapLastMod = Nothing,
              sitemapChangeFreq = Nothing,
              sitemapPriority = Just 0.4
            }
      )

getRobotsR :: Handler Text
getRobotsR = robots SitemapR
