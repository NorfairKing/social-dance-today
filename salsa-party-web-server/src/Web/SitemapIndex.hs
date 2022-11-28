{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.SitemapIndex where

import Conduit
import qualified Data.Conduit.List as CL
import Data.Default (def)
import Data.Text (Text)
import Data.Time
import Data.XML.Types
import Text.XML.Stream.Render (renderBuilder)
import Yesod.Core

data SitemapIndexUrl url = SitemapIndexUrl
  { sitemapIndexLoc :: url,
    sitemapIndexLastMod :: Maybe UTCTime
  }

-- | Serve a stream of @SitemapIndexUrl@s as a sitemap.
--
-- Since 1.2.0
sitemapIndex ::
  ConduitT () (SitemapIndexUrl (Route site)) (HandlerFor site) () ->
  HandlerFor site TypedContent
sitemapIndex urls = do
  render <- getUrlRender
  respondSource typeXml $ do
    yield Flush
    urls .| sitemapIndexConduit render .| renderBuilder def .| CL.map Chunk

-- | Convenience wrapper for @sitemapIndex@ for the case when the input is an
-- in-memory list.
--
-- Since 1.2.0
sitemapIndexList :: [SitemapIndexUrl (Route site)] -> HandlerFor site TypedContent
sitemapIndexList = sitemapIndex . mapM_ yield

-- | Convert a stream of @SitemapIndexUrl@s to XML @Event@s using the given URL
-- renderer.
--
-- This function is fully general for usage outside of Yesod.
--
-- Since 1.2.0
sitemapIndexConduit ::
  Monad m =>
  (a -> Text) ->
  ConduitT (SitemapIndexUrl a) Event m ()
sitemapIndexConduit render = do
  yield EventBeginDocument
  element "sitemapindex" [] $ awaitForever goUrl
  yield EventEndDocument
  where
    namespace = "http://www.sitemaps.org/schemas/sitemap/0.9"
    element name' attrs inside = do
      yield $ EventBeginElement name attrs
      () <- inside
      yield $ EventEndElement name
      where
        name = Name name' (Just namespace) Nothing

    goUrl SitemapIndexUrl {..} = element "sitemap" [] $ do
      element "loc" [] $ yield $ EventContent $ ContentText $ render sitemapIndexLoc
      case sitemapIndexLastMod of
        Nothing -> pure ()
        Just lm -> element "lastmod" [] $ yield $ EventContent $ ContentText $ formatW3 lm
