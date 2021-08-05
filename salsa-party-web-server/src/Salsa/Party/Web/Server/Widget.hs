{-# LANGUAGE QuasiQuotes #-}

module Salsa.Party.Web.Server.Widget where

import Data.Default
import Language.Haskell.TH.Syntax (Exp, Q)
import Salsa.Party.Web.Server.Constants
import Yesod
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFile :: String -> Q Exp
widgetFile =
  if development
    then widgetFileReload widgetFileSettings
    else widgetFileNoReload widgetFileSettings

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

genToken :: MonadHandler m => m Html
genToken = do
  alreadyExpired
  req <- getRequest
  let tokenKey = defaultCsrfParamName
  pure $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
