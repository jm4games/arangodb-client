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
  , createCollection
  , createOrGetCollection
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Types

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data CollectionError
  = ColErrNoName
  | ColErrInvalidRequest
  | ColErrAlreadyExist
  | ColErrUnknown HTTP.Status T.Text
  deriving Show

data CollectionConfig = CollectionConfig
  { ccName        :: !Name
  , ccType        :: !CollectionType
  , ccWaitForSync :: !Bool
  , ccKeyOptions  :: !KeyOption
  }

-- brittany-disable-next-binding
instance A.ToJSON CollectionConfig where
  toJSON c = A.object $
    [ "name" A..= ccName c
    , "waitForSync" A..= ccWaitForSync c
    , "keyOptions" A..= ccKeyOptions c
    ] <> toJSONCollectiontype (ccType c)

  toEncoding c = A.pairs
    (  "name" A..= ccName c
    <> "waitForSync" A..= ccWaitForSync c
    <> "keyOptions" A..= ccKeyOptions c
    <> toEncodingCollectiontype (ccType c)
    )

type AllowUserKeys = Bool
type Increment = Int
type Offset = Int
type Buckets = Int

data KeyOption
  = KeyOptTraditional !AllowUserKeys
  | KeyOptAutoIncrement !AllowUserKeys !Increment !Offset
  | KeyOptPadded !AllowUserKeys
  | KeyOptUUID !AllowUserKeys

instance A.ToJSON KeyOption where
  toJSON (KeyOptTraditional a) = A.object ["allowUserKeys" A..= a]
  toJSON (KeyOptAutoIncrement a i o) =
    A.object ["allowUserKeys" A..= a, "increment" A..= i, "offset" A..= o]
  toJSON (KeyOptPadded a) = A.object ["allowUserKeys" A..= a]
  toJSON (KeyOptUUID   a) = A.object ["allowUserKeys" A..= a]

  toEncoding (KeyOptTraditional a) = A.pairs ("allowUserKeys" A..= a)
  toEncoding (KeyOptAutoIncrement a i o) =
    A.pairs ("allowUserKeys" A..= a <> "increment" A..= i <> "offset" A..= o)
  toEncoding (KeyOptPadded a) = A.pairs ("allowUserKeys" A..= a)
  toEncoding (KeyOptUUID   a) = A.pairs ("allowUserKeys" A..= a)

data CollectionType = ColTypeDocument | ColTypeEdgeCollection !(Maybe Buckets)

toJSONCollectiontype :: CollectionType -> [(T.Text, A.Value)]
toJSONCollectiontype ColTypeDocument = ["type" A..= (2 :: Int)]
toJSONCollectiontype (ColTypeEdgeCollection mBuckets) =
  ["type" A..= (3 :: Int)] <> maybe [] (\b -> ["indexBuckets" A..= b]) mBuckets

toEncodingCollectiontype :: CollectionType -> A.Series
toEncodingCollectiontype ColTypeDocument = "type" A..= (2 :: Int)
toEncodingCollectiontype (ColTypeEdgeCollection mBuckets) =
  ("type" A..= (3 :: Int)) <> maybe mempty ("indexBuckets" A..=) mBuckets

mkColReq :: Database -> Name -> MkReq
mkColReq db n p = dbMkReq db ("/collection/" <> bs <> p) where bs = encodeUtf8 n

createCollection
  :: Client -> Database -> CollectionConfig -> IO (Either CollectionError Collection)
createCollection c db cfg = do
  res <- HTTP.httpLbs req (cManager c)
  print (HTTP.responseBody res)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right (Collection (ccName cfg) (mkColReq db (ccName cfg)))
    x | x == HTTP.status400 -> Left ColErrNoName
    x | x == HTTP.status404 -> Left ColErrInvalidRequest
    x | x == HTTP.status409 -> Left ColErrAlreadyExist
    x                       -> Left (ColErrUnknown x (readErrorMessage res))
 where
  req = (dbMkReq db "/_api/collection")
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode cfg)
    }

createOrGetCollection
  :: Client -> Database -> CollectionConfig -> IO (Either CollectionError Collection)
createOrGetCollection c db cfg = do
  res <- createCollection c db cfg
  let n = ccName cfg
  case res of
    Left ColErrAlreadyExist -> return (Right $ Collection n (mkColReq db n))
    _                       -> return res
