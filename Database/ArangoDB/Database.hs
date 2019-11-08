module Database.ArangoDB.Database
  ( DatabaseError(..)
  , Database
  , createDatabase
  , createOrGetDatabase
  )
where

import Database.ArangoDB.Internal
import Database.ArangoDB.Types

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data DatabaseError
  = DbErrInvalidRequest T.Text
  | DbErrSystemFailure
  | DbErrAlreadyExist
  | DbErrUnknown HTTP.Status T.Text
  deriving Show

mkDbReq :: Name -> HTTP.Request -> MkReq
mkDbReq n req = \p -> req { HTTP.path = "/_db/" <> bs <> p } where bs = encodeUtf8 n

createDatabase :: Client -> Name -> [User] -> IO (Either DatabaseError Database)
createDatabase c n users = do
  res <- HTTP.httpLbs req (cManager c)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status201 -> Right (Database n (mkDbReq n (cJsonReq c)))
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
  case res of
    Left DbErrAlreadyExist -> return (Right $ Database n (mkDbReq n (cJsonReq c)))
    _                       -> return res
