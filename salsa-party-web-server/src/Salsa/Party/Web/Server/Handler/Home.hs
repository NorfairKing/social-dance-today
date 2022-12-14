{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds -fno-warn-incomplete-uni-patterns #-}

module Salsa.Party.Web.Server.Handler.Home
  ( getHomeR,
    socialDanceWebSite,
    socialDanceOrganisation,
  )
where

import Salsa.Party.Web.Server.Handler.Import
import Web.JSONLD as LD

getHomeR :: Handler Html
getHomeR =
  withNavBar $ do
    setTitle $ toHtml siteTitle
    setDescriptionIdempI $ MsgHomeDescription siteTitle
    renderUrl <- getUrlRender
    toWidgetHead $ toJSONLDData $ socialDanceWebSite renderUrl
    toWidgetHead $ toJSONLDData $ socialDanceOrganisation renderUrl
    let queryId = "query"
    let statusId = "status"
    let helpId = "help"
    $(widgetFile "home") <> locateMeButton queryId statusId helpId

socialDanceWebSite :: (Route App -> Text) -> LD.WebSite
socialDanceWebSite renderUrl =
  let queryVariable = "search_term_string"
   in LD.WebSite
        { LD.webSiteUrl = Just $ renderUrl HomeR,
          LD.webSiteName = Just "Social Dance Today",
          LD.webSitePotentialActions =
            [ LD.SearchAction
                { LD.searchActionTarget =
                    LD.EntryPoint
                      { entryPointUrlTemplate =
                          -- We would like to use 'Search ""' instead of
                          -- HomeR here but if we do that, Yesod adds a
                          -- dash.  We can't just use the curly brackets in
                          -- the search query either, because they'll get
                          -- encoded.  Instead, we just make sure that we
                          -- are being pointed to this code again if the
                          -- SearchR route changes, and doing it in this
                          -- hacky  way with HomeR instead, with a golden
                          -- test.
                          let SearchR _ = undefined
                           in renderUrl HomeR <> "{" <> queryVariable <> "}"
                      },
                  searchActionQueryInput = "required name=" <> queryVariable
                }
            ]
        }

socialDanceOrganisation :: (Route App -> Text) -> LD.Organization
socialDanceOrganisation renderUrl =
  LD.Organization
    { LD.organizationName = "Social Dance Today",
      LD.organizationUrl = Just $ renderUrl HomeR,
      LD.organizationLogo = Just $ renderUrl LogoR,
      LD.organizationFounder =
        Just
          Person
            { personName = Just "Tom Sydney Kerckhove",
              personUrl = Just "https://cs-syd.eu",
              personJobTitle = Just "Professional Weirdo",
              personAffiliation = Just "CS SYD",
              personBirthDate = Just "1994-07-25",
              personAlumniOf = Just "ETH Zuerich",
              personBrand = Just "CS SYD",
              personGender = Just "male",
              personNationality = Just "Belgian"
            }
    }
