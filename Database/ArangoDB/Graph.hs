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

mkGphReq :: Database -> Name -> MkReq
mkGphReq db n p = dbMkReq db ("/gharial/" <> bs <> p) where bs = encodeUtf8 n

createGraph
  :: Client -> Database -> GraphConfig -> IO (Either GraphError Graph)
createGraph c db cfg = do
  res <- HTTP.httpLbs req (cManager c)
  print (HTTP.responseBody res)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right (Graph (gName cfg) (mkGphReq db (gName cfg)))
    x | x == HTTP.status202 -> Right (Graph (gName cfg) (mkGphReq db (gName cfg)))
    x | x == HTTP.status400 -> Left (GphErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status403 -> Left GphErrForbidden
    x | x == HTTP.status409 -> Left GphErrAlreadyExist
    x                       -> Left (GphErrUnknown x (readErrorMessage res))
 where
  req = (dbMkReq db "/_api/gharial")
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode cfg)
    }

createOrGetGraph
  :: Client -> Database -> GraphConfig -> IO (Either GraphError Graph)
createOrGetGraph c db cfg = do
  res <- createGraph c db cfg
  let n = gName cfg
  case res of
    Left GphErrAlreadyExist -> return (Right $ Graph n (mkGphReq db n))
    _                       -> return res
