{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
  acqOrganisers <- runDB $ selectSourceRes [] [Asc OrganiserId]

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
    forM_ locations $ \location -> do
      forM_ allDanceStyles $ \danceStyle ->
        yield
          SitemapUrl
            { sitemapLoc = SearchDanceStyleR (placeQuery $ locationPlace location) danceStyle,
              sitemapLastMod = Just $ UTCTime today 0, -- At the beginning of the day
              sitemapChangeFreq = Just Daily,
              sitemapPriority = Just 0.5
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
              sitemapPriority = Just 0.4
            }
      )

getRobotsR :: Handler TL.Text
getRobotsR = do
  urlRender <- getUrlRenderParams
  pure $ TLB.toLazyText $ $(textFile "templates/robots.txt") urlRender
