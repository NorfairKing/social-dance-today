{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Static where

import Path
import Salsa.Party.Web.Server.Static.TH

mkStatic

mkRuntimeStaticDir
