module Database.ArangoDB.Types where

import Data.ByteString (ByteString)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

type Name = T.Text
type Path = ByteString
type CollectionName = Name

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

newtype Rev = Rev T.Text

instance A.FromJSON Rev where
  parseJSON (A.String x) = return (Rev x)
  parseJSON invalid = A.typeMismatch "Rev" invalid
