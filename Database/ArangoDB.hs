module Database.ArangoDB (Client, newClient) where

import Data.Maybe (fromMaybe)

import Database.ArangoDB.Internal (Client(..))

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP

newClient :: String -> Maybe HTTP.ManagerSettings -> IO Client
newClient addr mSettings = do
  mgr <- HTTP.newManager (fromMaybe HTTP.defaultManagerSettings mSettings)
  req <- HTTP.setRequestIgnoreStatus <$> HTTP.parseRequest addr
  return $ Client addr (jReq req) mgr
 where
  jHeader = (HTTP.hContentType, "application/rdf")
  jReq req = req { HTTP.requestHeaders = jHeader : HTTP.requestHeaders req }
