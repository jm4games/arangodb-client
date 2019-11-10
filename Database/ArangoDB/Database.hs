module Database.ArangoDB.Database
  ( DatabaseError(..)
  , Database
  , createDatabase
  , createOrGetDatabase
  , getDatabase
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Types

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data DatabaseError
  = DbErrInvalidRequest !T.Text
  | DbErrSystemFailure
  | DbErrAlreadyExist
  | DbErrNotFound
  | DbErrUnknown !HTTP.Status !T.Text
  deriving Show

mkDbReq :: BS.ByteString -> HTTP.Request -> MkReq
mkDbReq n req p = req { HTTP.path = "/_db/" <> n <> p }

createDatabase :: Client -> Name -> [User] -> IO (Either DatabaseError Database)
createDatabase c n users = do
  res <- HTTP.httpLbs req (cManager c)
  let n' = encodeUtf8 n
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status201 -> Right (Database n' (mkDbReq n' (cJsonReq c)) c)
    x | x == HTTP.status400 -> Left (DbErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status403 -> Left DbErrSystemFailure
    x | x == HTTP.status409 -> Left DbErrAlreadyExist
    x                       -> Left (DbErrUnknown x (readErrorMessage res))
 where
  body = HM.fromList [("name" :: T.Text, A.String n), ("users", A.toJSON users)]
  req  = (cJsonReq c)
    { HTTP.path        = "/_api/database"
    , HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS (A.encode body)
    }

createOrGetDatabase :: Client -> Name -> [User] -> IO (Either DatabaseError Database)
createOrGetDatabase c n users = do
  res <- createDatabase c n users
  let n' = encodeUtf8 n
  case res of
    Left DbErrAlreadyExist -> return (Right $ Database n' (mkDbReq n' (cJsonReq c)) c)
    _                       -> return res

getDatabase :: Client -> Name -> IO (Either DatabaseError Database)
getDatabase c n = do
  res <- HTTP.httpLbs req (cManager c)
  let n' = encodeUtf8 n
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> Right (Database n' (mkDbReq n' (cJsonReq c)) c)
    x | x == HTTP.status400 -> Left (DbErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status404 -> Left DbErrNotFound
    x                       -> Left (DbErrUnknown x (readErrorMessage res))
 where
  req  = (cJsonReq c)
    { HTTP.path        = "/_db/" <> encodeUtf8 n <> "/_api/database"
    , HTTP.method      = HTTP.methodGet
    }


