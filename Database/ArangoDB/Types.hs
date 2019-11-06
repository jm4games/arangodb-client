module Database.ArangoDB.Types where

import Data.ByteString (ByteString)

import qualified Data.Aeson as A
import qualified Data.Text as T

type Name = T.Text
type Path = ByteString

data User = User
  { uUsername :: !T.Text
  , uPassword :: !T.Text
  , uActive   :: !Bool
  , uExtra    :: !A.Value
  }

instance A.ToJSON User where
  toJSON u = A.object
    [ "username" A..= uUsername u
    , "password" A..= uPassword u
    , "active" A..= uActive u
    , ("extra", uExtra u)
    ]

  toEncoding u = A.pairs
    (  "username" A..= uUsername u
    <> "password" A..= uPassword u
    <> "active" A..= uActive u
    <> "extra" A..= uExtra u
    )
