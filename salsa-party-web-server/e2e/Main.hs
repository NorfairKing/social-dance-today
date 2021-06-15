module Main where

import Control.Monad.Logger
import LinkCheck (runLinkCheck)
import qualified LinkCheck.OptParse.Types as LinkCheck (Settings (..))
import Network.URI
import System.Environment
import System.Exit
import Test.Syd

main :: IO ()
main = do
  let var = "SALSA_PARTY_SERVER_URL"
  ms <- lookupEnv var
  case ms of
    Nothing -> die $ "URL not configured using: " <> var
    Just s -> case parseAbsoluteURI s of
      Nothing -> die $ "Not an absolute URI: " <> s
      Just uri -> sydTest $ spec uri

spec :: URI -> Spec
spec uri = do
  sequential $
    it "passes linkcheck" $ do
      runLinkCheck
        LinkCheck.Settings
          { LinkCheck.setUri = uri,
            LinkCheck.setLogLevel = LevelWarn,
            LinkCheck.setFetchers = Nothing,
            LinkCheck.setExternal = False,
            LinkCheck.setCheckFragments = False
          }
