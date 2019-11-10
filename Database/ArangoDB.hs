module Database.ArangoDB
  ( module X
  , Client
  , newClient
  ) where

import Data.Maybe (fromMaybe)

import Database.ArangoDB.Internal (Client(..))

import Database.ArangoDB.Collection as X
import Database.ArangoDB.Database as X
import Database.ArangoDB.Document as X
import Database.ArangoDB.Graph as X
import Database.ArangoDB.Index as X
import Database.ArangoDB.Key as X
import Database.ArangoDB.Types as X

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
