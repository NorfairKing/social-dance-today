{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Prospect where

import Salsa.Party.Web.Server.Handler.Import

getUnsubProspectR :: ProspectSecret -> Handler Html
getUnsubProspectR secret = do
  mProspect <- runDB $ getBy $ UniqueProspectSecret secret
  case mProspect of
    Nothing -> notFound
    Just (Entity prospectId _) -> do
      now <- liftIO getCurrentTime
      runDB $ update prospectId [ProspectUnsubscribed =. Just now]
      withNavBar $ do
        setTitleI MsgUnsubProspectTitle
        setDescriptionIdempI MsgUnsubProspectDescription
        $(widgetFile "unsub/prospect")
