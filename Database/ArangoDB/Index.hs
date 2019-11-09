module Database.ArangoDB.Index
  ( Fields
  , MinLength
  , GeoJson
  , Unique
  , Sparse
  , Deduplicate
  , ExpiresAfter
  , Index
  , IndexType(..)
  , IndexError(..)
  , createIndex
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Types (Name, CollectionName)

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

type Fields = [Name]
type MinLength = Int
type GeoJson = Bool
type Unique = Bool
type Sparse = Bool
type Deduplicate = Bool
type ExpiresAfter = Int -- Seconds

data IndexType
  = IdxFullText !MinLength
  | IdxGeo !GeoJson
  | IdxHash !Unique !Sparse !Deduplicate
  | IdxPersistent !Unique !Sparse
  | IdxSkipList !Unique !Sparse !Deduplicate
  | IdxTTL !ExpiresAfter

toEncodingIndexType :: IndexType -> A.Series
toEncodingIndexType it = case it of
  IdxFullText mLen -> mkType "fulltext" <> "minLength" A..= mLen
  IdxGeo      geo  -> mkType "geo" <> "geoJson" A..= geo
  IdxHash u s d ->
    mkType "hash" <> "unique" A..= u <> "sparse" A..= s <> "deduplicate" A..= d
  IdxPersistent u s -> mkType "persistent" <> "unique" A..= u <> "sparse" A..= s
  IdxSkipList u s d ->
    mkType "skiplist" <> "unique" A..= u <> "sparse" A..= s <> "deduplicate" A..= d
  IdxTTL e -> mkType "ttl" <> "expireAfter" A..= e
 where
  mkType :: T.Text -> A.Series
  mkType t = "type" A..= t

data IndexCfg = IndexCfg { cfgType :: IndexType, cfgFields :: Fields }

instance A.ToJSON IndexCfg where
  toJSON = undefined
  toEncoding cfg =
    A.pairs (toEncodingIndexType (cfgType cfg) <> "fields" A..= cfgFields cfg)

data IndexError
  = IdxErrInvalidRequest !T.Text
  | IdxErrUnknownCollection
  | IdxErrUnknown !HTTP.Status !T.Text

data Index = Index

createIndex
  :: Client
  -> Database
  -> CollectionName
  -> IndexType
  -> Fields
  -> IO (Either IndexError Index)
createIndex c db cn idx fields = do
  res <- HTTP.httpLbs req (cManager c)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right Index
    x | x == HTTP.status201 -> Right Index
    x | x == HTTP.status400 -> Left (IdxErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status404 -> Left IdxErrUnknownCollection
    x                       -> Left (IdxErrUnknown x (readErrorMessage res))
 where
  req = (dbMkReq db ("/_api/index?collection=" <> encodeUtf8 cn))
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode (IndexCfg idx fields))
    }
