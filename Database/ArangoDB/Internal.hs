{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Database.ArangoDB.Internal where

import Database.ArangoDB.Key
import Database.ArangoDB.Types

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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
  { dbName   :: !BS.ByteString
  , dbMkReq  :: !MkReq
  , dbClient :: !Client
  }

instance Show Database where
  show db = "Database{" <> BS8.unpack (dbName db) <> "}"

data Collection (k :: KeyType) = Collection
  { colName  :: !BS.ByteString
  , colMkReq :: !MkReq
  , colDb    :: !Database
  }

colManager :: Collection k -> HTTP.Manager
colManager = cManager . dbClient . colDb

instance Show (Collection k) where
  show c = "Collection{" <> BS8.unpack (colName c) <> "}"

data Graph = Graph
  { gphName  :: !BS.ByteString
  , gphMkReq :: !MkReq
  }

instance Show Graph where
  show c = "Graph{" <> BS8.unpack (gphName c) <> "}"

newtype ErrorBody = ErrorBody { unwrapErrBody :: T.Text }

instance A.FromJSON ErrorBody where
  parseJSON (A.Object v) = ErrorBody <$> v A..: "errorMessage"
  parseJSON invalid = A.typeMismatch "ErrorBody" invalid

readErrorMessage :: HTTP.Response LBS.ByteString -> T.Text
readErrorMessage = maybe "" unwrapErrBody . A.decode . HTTP.responseBody
