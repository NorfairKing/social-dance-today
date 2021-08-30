module Main where

import Salsa.Party.Web.Server.TestUtils.Selenium
import Spec
import Test.Syd

main :: IO ()
main = sydTest $ webdriverSpec spec
