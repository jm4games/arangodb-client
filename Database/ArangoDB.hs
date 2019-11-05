module Database.ArangoDB where

import Database.ArangoDB.Database (DatabaseError, Database)

import qualified Data.ByteString.Lazy as LBS

import qualified Network.HTTP.Client as HTTP

newtype Client = Client
  { cManager :: !HTTP.Manager
  }

data User = User
  { uUsername :: !T.Text
  , uPassword :: !T.Text
  , uActive   :: !Boolean
  , uExtra    :: !LBS.ByteString
  }

createDatabase :: Client -> Name -> [User] -> IO (Either ErrorDatabase Database)
createDatabase c n users = undefined
