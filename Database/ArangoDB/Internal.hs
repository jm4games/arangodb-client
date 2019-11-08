module Database.ArangoDB.Internal where

import Database.ArangoDB.Types

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
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

newtype ErrorBody = ErrorBody { unwrapErrBody :: T.Text }

instance A.FromJSON ErrorBody where
  parseJSON (A.Object v) = ErrorBody <$> v A..: "errorMessage"
  parseJSON invalid = A.typeMismatch "ErrorBody" invalid

readErrorMessage :: HTTP.Response LBS.ByteString -> T.Text
readErrorMessage = maybe "" unwrapErrBody . A.decode . HTTP.responseBody
