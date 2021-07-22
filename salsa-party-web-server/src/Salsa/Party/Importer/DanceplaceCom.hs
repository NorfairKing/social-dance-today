{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://danceplace.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable.
--
-- The robots.txt contains a line that mentions a sitemap which is a text file.
--
-- > https://www.danceplace.com/robots.txt:
--
-- > User-Agent: *
-- > Allow: /
-- >
-- > Sitemap: https://www.danceplace.com/assets/site-map-31-07-20.txt
--
-- Not sure whether that's allowed but that text file contains all links to all events.
--
-- The text file contains an index of everything on the site, including events.
-- * Urls like this:
--   https://www.danceplace.com/index/no/12/Flying+Dog-Waterloo_+ON-Canada-Place+to+social+dance+Salsa
--   Which are weird, events from 60 years ago?!
--
-- * Urls that end in +school, which look like schools.
--   https://www.danceplace.com/index/no/626/Air+De+Tango+-Montreal_+QC-Canada-Tango+Dance+school
--
-- * Urls that end in +event, which look like events:
--   https://www.danceplace.com/index/no/8177/BachataStars+Poland-2020-Warsaw-Poland-Bachata+Dance+event
--   The event urls also always seem to contain the year that they're in.
--
-- The event pages are not machine readible, but there are some nice benefits anyway.
--
-- * the title has a <span itemprop="name">
-- * the description has a <meta itemprop="description">
-- * The start as <meta itemprop="startDate" content="2021-03-05T00:00">
-- * The end date as <meta itemprop="endDate" content="2021-03-07T00:00">
module Salsa.Party.Importer.DanceplaceCom (danceplaceComImporter) where

import Conduit
import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Retry as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import qualified Text.HTML.TagSoup as HTML
import qualified Text.HTML.TagSoup.Match as HTML
import qualified Web.JSONLD as LD
import qualified Web.JSONLD.Parse as LD

danceplaceComImporter :: Importer
danceplaceComImporter =
  Importer
    { importerName = "danceplace.com",
      importerFunc = func
    }

baseUrl = "https://danceplace.com"

func :: Import ()
func = do
  now <- liftIO getCurrentTime
  let (currentYear, _, _) = toGregorian $ utctDay now
  case parseRequest $ baseUrl <> "/robots.txt" of
    Nothing -> logErrorN "Robots.txt url was invalid."
    Just request -> do
      errOrResponse <- doHttpRequest request
      case errOrResponse of
        Left err -> logErrorN $ T.pack $ "Could not reach robots.txt:\n" <> ppShow err
        Right response -> do
          let sitemapUrls = mapMaybe (SB.stripPrefix "Sitemap: ") $ SB8.lines $ LB.toStrict $ responseBody response
          runConduit $
            yieldMany sitemapUrls
              .| C.concatMap (parseRequest . SB8.unpack :: ByteString -> Maybe Request)
              .| doHttpRequestWith
              .| logRequestErrors
              .| C.map (responseBody . snd)
              .| C.splitOnUnboundedE (== 0x0a)
              .| C.filter ("+event" `LB8.isSuffixOf`)
              .| C.map LB.toStrict
              .| C.filter
                ( \url ->
                    SB8.pack (show currentYear) `SB8.isInfixOf` url
                      || SB8.pack (show (succ currentYear)) `SB8.isInfixOf` url
                )
              .| C.concatMap (parseRequest . SB8.unpack :: ByteString -> Maybe Request)
              .| doHttpRequestWith
              .| logRequestErrors
              .| C.mapM_ (uncurry parseEventFromPage)

parseEventFromPage :: HTTP.Request -> HTTP.Response LB.ByteString -> Import ()
parseEventFromPage request response = do
  externalEventUuid <- nextRandomUUID
  let externalEventKey =
        let uriText = T.pack $ show $ getUri request
         in case T.stripPrefix "https://www.danceplace.com/index/no/" uriText of
              Nothing -> uriText
              Just suffix -> suffix
  let tags = HTML.parseTags $ responseBody response

      maybeUtf8 sb = case TE.decodeUtf8' (LB.toStrict sb) of
        Left _ -> Nothing
        Right t -> Just t

  let parseMaybeUnder :: String -> LB.ByteString -> ([HTML.Attribute LB.ByteString] -> Bool) -> [HTML.Tag LB.ByteString] -> Maybe Text
      parseMaybeUnder partName name attrMatch tags_ =
        let relevantTags :: [HTML.Tag LB.ByteString]
            relevantTags = getTagContentSafe name attrMatch tags_
            relevantContents :: Text
            relevantContents = T.intercalate " " $ map T.strip $ mapMaybe maybeUtf8 $ mapMaybe HTML.maybeTagText relevantTags
            text = T.strip relevantContents
         in if T.null text
              then Nothing
              else Just text

  let parseMaybe :: String -> LB.ByteString -> ([HTML.Attribute LB.ByteString] -> Bool) -> Maybe Text
      parseMaybe partName name attrMatch = parseMaybeUnder partName name attrMatch tags

  let maybeItemPropContents :: LB.ByteString -> Maybe LB.ByteString
      maybeItemPropContents name =
        listToMaybe $
          flip mapMaybe tags $ \case
            HTML.TagOpen "meta" attrs -> do
              propName <- lookup "itemprop" attrs
              guard $ propName == name
              lookup "content" attrs
            _ -> Nothing

  let parseDateTimeAnd :: String -> LB.ByteString -> (Day -> Import ()) -> Import ()
      parseDateTimeAnd partName name func = do
        let mDay = do
              contents <- maybeItemPropContents name
              text <- maybeUtf8 contents
              parseTimeM True defaultTimeLocale "%FT%H:%M" $ T.unpack text
        case mDay of
          Nothing -> logWarnN $ T.pack $ "Piece not found: " <> partName
          Just day -> func day

  let mTitle = do
        let relevantTags = getTagContentSafe "div" (HTML.anyAttrLit ("class", "modal-header")) tags
        parseMaybeUnder "title" "h3" (const True) relevantTags

  today <- liftIO $ utctDay <$> getCurrentTime
  parseDateTimeAnd "day" "startDate" $ \externalEventDay ->
    if externalEventDay < today -- No point in getting old events.
      then pure ()
      else do
        case mTitle of
          Nothing -> logWarnN "Unable to parse title"
          Just externalEventTitle ->
            case parseMaybe
              "address"
              "a"
              ( \ls ->
                  HTML.anyAttrLit ("target", "_blank") ls
                    && HTML.anyAttr (\(name, val) -> name == "href" && "https://maps.google.com/" `LB8.isPrefixOf` val) ls
              ) of
              Nothing -> logWarnN "Unable to parse address"
              Just rawAddress -> do
                let externalEventDescription = maybeItemPropContents "description" >>= maybeUtf8
                let externalEventOrganiser = Nothing -- TODO this is sometimes on the page
                let externalEventStart = Nothing
                let externalEventHomepage = maybeItemPropContents "url" >>= maybeUtf8
                let externalEventPrice = Nothing
                let externalEventCancelled = case parseMaybe "attendance" "meta" (HTML.anyAttrLit ("itemprop", "eventStatus")) of
                      Just "http://schema.org/EventPostponed" -> True -- TODO change this once we implement postponing
                      Just "http://schema.org/EventCancelled" -> True
                      _ -> False

                now <- liftIO getCurrentTime
                let externalEventCreated = now
                let externalEventModified = Nothing
                app <- asks importEnvApp
                let address = T.replace " , " ", " $ T.strip rawAddress
                mPlaceEntity <- runReaderT (lookupPlaceRaw address) app
                case mPlaceEntity of
                  Nothing -> logWarnN $ "Place not found: " <> address
                  Just (Entity externalEventPlace _) -> do
                    externalEventImporter <- Just <$> asks importEnvId
                    let externalEventOrigin = T.pack $ show $ getUri request

                    importExternalEventAnd ExternalEvent {..} $ \externalEventId -> do
                      forM_ (maybeItemPropContents "image" >>= maybeUtf8 >>= (parseURI . T.unpack)) $ \uri -> do
                        mImageId <- tryToImportImage uri
                        forM_ mImageId $ \imageId -> do
                          importDB $
                            upsertBy
                              (UniqueExternalEventPoster externalEventId)
                              ( ExternalEventPoster
                                  { externalEventPosterExternalEvent = externalEventId,
                                    externalEventPosterImage = imageId,
                                    externalEventPosterCreated = now,
                                    externalEventPosterModified = Nothing
                                  }
                              )
                              [ ExternalEventPosterImage =. imageId,
                                ExternalEventPosterModified =. Just now
                              ]

-- Refactor this piece
getTagContentSafe :: Eq str => str -> ([HTML.Attribute str] -> Bool) -> [HTML.Tag str] -> [HTML.Tag str]
getTagContentSafe name pAttrs tags =
  case listToMaybe $ sections (HTML.tagOpenLit name pAttrs) tags of
    Nothing -> []
    Just l -> takeWhile (not . HTML.tagCloseLit name) $ drop 1 l

sections :: (a -> Bool) -> [a] -> [[a]]
sections p = \case
  [] -> []
  l -> filter (p . head) $ init $ tails l
