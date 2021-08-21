{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Home (getHomeR, socialDanceOrganisation) where

import Salsa.Party.Web.Server.Handler.Import
import Web.JSONLD as LD

getHomeR :: Handler Html
getHomeR = do
  messageRender <- getMessageRender
  withNavBar $ do
    setTitleI MsgHomeTitle
    setDescriptionI MsgHomeDescription
    renderUrl <- getUrlRender
    toWidgetHead $ toJSONLDData $ socialDanceOrganisation renderUrl
    $(widgetFile "home")

socialDanceOrganisation :: (Route App -> Text) -> LD.Organization
socialDanceOrganisation renderUrl =
  LD.Organization
    { LD.organizationName = "Social Dance Today",
      organizationUrl = Just $ renderUrl HomeR,
      organizationLogo = Just $ renderUrl (StaticR logo_svg),
      organizationFounder = Just "Tom Sydney Kerckhove"
    }
