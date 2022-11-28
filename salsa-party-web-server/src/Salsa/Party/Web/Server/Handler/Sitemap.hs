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
    getRobotsR,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import
import Text.Shakespeare.Text
import Web.SitemapIndex
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR =
  sitemapIndexList
    [ SitemapIndexUrl
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
    ]

getSitemapTopLevelR :: Handler TypedContent
getSitemapTopLevelR = do
  today <- liftIO $ utctDay <$> getCurrentTime
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

  let dbAcq ::
        Acquire (ConduitM () a Handler ()) ->
        (a -> SitemapUrl (Route App)) ->
        ConduitT () (SitemapUrl (Route App)) Handler ()
      dbAcq acq func = do
        (rk, aSource) <- allocateAcquire acq
        aSource
          .| C.map func
        release rk
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

getRobotsR :: Handler TL.Text
getRobotsR = do
  urlRender <- getUrlRenderParams
  pure $ TLB.toLazyText $ $(textFile "templates/robots.txt") urlRender
