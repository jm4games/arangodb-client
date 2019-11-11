module Database.ArangoDB.Graph
  ( GraphError(..)
  , GraphConfig(..)
  , Graph
  , EdgeDefinition (..)
  , CollectionName
  , createGraph
  , createOrGetGraph
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Types

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data GraphError
  = GphErrInvalidRequest T.Text
  | GphErrForbidden
  | GphErrAlreadyExist
  | GphErrUnknown HTTP.Status T.Text
  deriving Show

data GraphConfig = GraphConfig
  { gName        :: !Name
  , gEdges       :: ![EdgeDefinition]
  }

-- brittany-disable-next-binding
instance A.ToJSON GraphConfig where
  toJSON c = A.object
    [ "name" A..= gName c
    , "edgeDefinitions" A..= gEdges c
    ]

  toEncoding c = A.pairs
    (  "name" A..= gName c
    <> "edgeDefinitions" A..= gEdges c
    )

data EdgeDefinition = EdgeDefinition
  { edTo             :: ![CollectionName]
  , edFrom           :: ![CollectionName]
  , edEdgeCollection :: !Name
  }

instance A.ToJSON EdgeDefinition where
  toJSON def = A.object
    [ "to" A..= edTo def
    , "from" A..= edFrom def
    , "collection" A..= edEdgeCollection def
    ]

  toEncoding def = A.pairs
    (  "to" A..= edTo def
    <> "from" A..= edFrom def
    <> "collection" A..= edEdgeCollection def
    )

mkGphReq :: Database -> BS.ByteString -> MkReq
mkGphReq db n p = dbMkReq db ("/gharial/" <> n <> p)

createGraph :: Database -> GraphConfig -> IO (Either GraphError Graph)
createGraph db cfg = do
  res <- HTTP.httpLbs req (cManager (dbClient db))
  let n = encodeUtf8 (gName cfg)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 || x == HTTP.status202 -> Right (Graph n (mkGphReq db n))
    x | x == HTTP.status400 -> Left (GphErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status403 -> Left GphErrForbidden
    x | x == HTTP.status409 -> Left GphErrAlreadyExist
    x                       -> Left (GphErrUnknown x (readErrorMessage res))
 where
  req = (dbMkReq db "/_api/gharial")
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode cfg)
    }

createOrGetGraph :: Database -> GraphConfig -> IO (Either GraphError Graph)
createOrGetGraph db cfg = do
  res <- createGraph db cfg
  let n = encodeUtf8 (gName cfg)
  case res of
    Left GphErrAlreadyExist -> return (Right $ Graph n (mkGphReq db n))
    _                       -> return res
