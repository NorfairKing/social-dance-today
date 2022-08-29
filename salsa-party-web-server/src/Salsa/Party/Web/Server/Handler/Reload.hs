module Salsa.Party.Web.Server.Handler.Reload (getReloadR) where

import Salsa.Party.Web.Server.Handler.Import
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadR
