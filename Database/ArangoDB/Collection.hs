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
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Types

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as A
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data CollectionError = ColErrNoName | ColErrInvalidRequest | ColErrUnknown HTTP.Status deriving Show

data CollectionConfig = CollectionConfig
  { ccName        :: !Name
  , ccType        :: !CollectionType
  , ccWaitForSync :: !Bool
  , ccKeyOptions  :: !KeyOption
  }

-- brittany-disable-next-binding
instance A.ToJSON CollectionConfig where
  toJSON c = A.object
    [ "name" A..= ccName c
    , "type" A..= ccType c
    , "waitForSync" A..= ccWaitForSync c
    , "keyOptions" A..= ccKeyOptions c
    ]

  toEncoding c = A.pairs
    (  "name" A..= ccName c
    <> "type" A..= ccType c
    <> "waitForSync" A..= ccWaitForSync c
    <> "keyOptions" A..= ccKeyOptions c
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

data CollectionType = ColTypeDocument | ColTypeEdgeCollection !Buckets

--instance A.ToJSON CollectionType where
  --toJSON ColTypeDocument = "type" A..= (2 :: Int)
  --toJSON ColTypeEdgeCollection buckets

createCollection
  :: Client -> Database -> CollectionConfig -> IO (Either CollectionError Collection)
createCollection c db cfg = do
  res <- HTTP.httpLbs req (cManager c)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right (Collection (ccName cfg) mkDbReq)
    x | x == HTTP.status400 -> Left ColErrNoName
    x | x == HTTP.status404 -> Left ColErrInvalidRequest
    x                       -> Left (ColErrUnknown x)
 where
  req = (cJsonReq c)
    { HTTP.path        = "/_api/collection"
    , HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode cfg)
    }

  mkDbReq :: MkReq
  mkDbReq p = dbMkReq db ("/collection/" <> bs <> p) where bs = encodeUtf8 (ccName cfg)
