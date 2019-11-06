module Database.ArangoDB.Internal where

import Database.ArangoDB.Types

import qualified Network.HTTP.Client as HTTP

data Client = Client
  { cRootURL :: !String
  , cJsonReq :: !HTTP.Request
  , cManager :: !HTTP.Manager
  }

data Database = Database
  { dbname  :: !Name
  , dbMkReq :: Path -> HTTP.Request
  }
