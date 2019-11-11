{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Database.ArangoDB.Collection
  ( CollectionError(..)
  , CollectionConfig(..)
  , Collection
  , AllowUserKeys
  , Increment
  , Offset
  , Buckets
  , KeyOption(..)
  , CollectionType(..)
  , collectionName
  , createCollection
  , createOrGetCollection
  , getCollection
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Key
import Database.ArangoDB.Types

import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data CollectionError
  = ColErrNoName
  | ColErrInvalidRequest T.Text
  | ColErrAlreadyExist
  | ColErrUnknown HTTP.Status T.Text
  deriving Show

data CollectionConfig (k :: KeyType) = CollectionConfig
  { ccName        :: !Name
  , ccType        :: !CollectionType
  , ccWaitForSync :: !Bool
  , ccKeyOptions  :: !(KeyOption k)
  }

-- brittany-disable-next-binding
instance A.ToJSON (CollectionConfig k) where
  toJSON c = A.object $
    [ "name" A..= ccName c
    , "waitForSync" A..= ccWaitForSync c
    , "keyOptions" A..= ccKeyOptions c
    ] <> toJSONCollectiontype (ccType c)

  toEncoding c = A.pairs
    (  "name" A..= ccName c
    <> "waitForSync" A..= ccWaitForSync c
    <> (case ccKeyOptions c of
           KeyOptNone -> mempty
           _ -> ("keyOptions" A..=) (ccKeyOptions c))
    <> toEncodingCollectiontype (ccType c)
    )

type AllowUserKeys = Bool
type Increment = Int
type Offset = Int
type Buckets = Int

-- brittany-disable-next-binding
data KeyOption k where
  KeyOptTraditional   ::AllowUserKeys -> KeyOption 'NumericKey
  KeyOptAutoIncrement ::AllowUserKeys -> Increment -> Offset -> KeyOption 'NumericKey
  KeyOptPadded        ::AllowUserKeys -> KeyOption 'Padded16Key
  KeyOptUUID          ::AllowUserKeys -> KeyOption 'UUIDKey
  KeyOptNone          ::KeyOption 'TextKey

instance A.ToJSON (KeyOption k) where
  toJSON (KeyOptTraditional a) = A.object ["allowUserKeys" A..= a]
  toJSON (KeyOptAutoIncrement a i o) =
    A.object ["allowUserKeys" A..= a, "increment" A..= i, "offset" A..= o]
  toJSON (KeyOptPadded a) = A.object ["allowUserKeys" A..= a]
  toJSON (KeyOptUUID   a) = A.object ["allowUserKeys" A..= a]
  toJSON KeyOptNone = error "Unexpected key option none"

  toEncoding (KeyOptTraditional a) = A.pairs ("allowUserKeys" A..= a)
  toEncoding (KeyOptAutoIncrement a i o) =
    A.pairs ("allowUserKeys" A..= a <> "increment" A..= i <> "offset" A..= o)
  toEncoding (KeyOptPadded a) = A.pairs ("allowUserKeys" A..= a)
  toEncoding (KeyOptUUID   a) = A.pairs ("allowUserKeys" A..= a)
  toEncoding KeyOptNone =  error "Unexpected key option none"

data CollectionType = ColTypeDocument | ColTypeEdge !(Maybe Buckets)

toJSONCollectiontype :: CollectionType -> [(T.Text, A.Value)]
toJSONCollectiontype ColTypeDocument = ["type" A..= (2 :: Int)]
toJSONCollectiontype (ColTypeEdge mBuckets) =
  ["type" A..= (3 :: Int)] <> maybe [] (\b -> ["indexBuckets" A..= b]) mBuckets

toEncodingCollectiontype :: CollectionType -> A.Series
toEncodingCollectiontype ColTypeDocument = "type" A..= (2 :: Int)
toEncodingCollectiontype (ColTypeEdge mBuckets) =
  ("type" A..= (3 :: Int)) <> maybe mempty ("indexBuckets" A..=) mBuckets

mkColReq :: Database -> BS.ByteString -> MkReq
mkColReq db n p = dbMkReq db ("/collection/" <> n <> p)

collectionName :: Collection k -> T.Text
collectionName = decodeUtf8 . colName

createCollection
  :: Database -> CollectionConfig k -> IO (Either CollectionError (Collection k))
createCollection db cfg = do
  res <- HTTP.httpLbs req (cManager (dbClient db))
  let n = encodeUtf8 (ccName cfg)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right (Collection n (mkColReq db n) db)
    x | x == HTTP.status400 -> Left ColErrNoName
    x | x == HTTP.status404 -> Left (ColErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status409 -> Left ColErrAlreadyExist
    x                       -> Left (ColErrUnknown x (readErrorMessage res))
 where
  req = (dbMkReq db "/_api/collection")
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode cfg)
    }

createOrGetCollection
  :: Database -> CollectionConfig k -> IO (Either CollectionError (Collection k))
createOrGetCollection db cfg = do
  res <- createCollection db cfg
  let n = encodeUtf8 (ccName cfg)
  case res of
    Left ColErrAlreadyExist -> return (Right $ Collection n (mkColReq db n) db)
    _                       -> return res

getCollection :: Database -> CollectionName -> IO (Either CollectionError (Collection k))
getCollection db name = do
  res <- HTTP.httpLbs req (cManager (dbClient db))
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right (Collection n (mkColReq db n) db)
    x | x == HTTP.status400 -> Left (ColErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status404 -> Left (ColErrInvalidRequest (readErrorMessage res))
    x                       -> Left (ColErrUnknown x (readErrorMessage res))
 where
  req = (dbMkReq db ("/_api/collection/" <> n)) { HTTP.method = HTTP.methodGet }
  n   = encodeUtf8 name
