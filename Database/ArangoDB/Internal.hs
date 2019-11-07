module Database.ArangoDB.Internal where

import Database.ArangoDB.Types

import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP

data Client = Client
  { cRootURL :: !String
  , cJsonReq :: !HTTP.Request
  , cManager :: !HTTP.Manager
  }

type MkReq = Path -> HTTP.Request

data Database = Database
  { dbName  :: !Name
  , dbMkReq :: !MkReq
  }

instance Show Database where
  show db = "Database{" <> T.unpack (dbName db) <> "}"

data Collection = Collection
  { colName  :: !Name
  , colMkReq :: !MkReq
  }

instance Show Collection where
  show c = "Collection{" <> T.unpack (colName c) <> "}"
