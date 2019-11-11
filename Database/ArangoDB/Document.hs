{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.ArangoDB.Document
  ( CreateOptions(..)
  , DocumentMeta(..)
  , DocumentError(..)
  , BulkImportOptions(..)
  , BulkImportResult(..)
  , BulkDocument(..)
  , BulkHeader(..)
  , DuplicateAction(..)
  , Edge(..)
  , defCreateOptions
  , defBulkImportOptions
  , createDocuments
  , bulkImport
  )
where

import Database.ArangoDB.Key
import Database.ArangoDB.Internal

import Data.List (intersperse)

import qualified Codec.Compression.GZip as GZip
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP
import qualified Network.HTTP.Types.Status as HTTP

data CreateOptions = CreateOptions
  { coWaitForSync :: !(Maybe Bool)
  , coReturnOld   :: !(Maybe Bool)
  , coSilent      :: !(Maybe Bool)
  , coOverwrite   :: !(Maybe Bool)
  }

defCreateOptions :: CreateOptions
defCreateOptions = CreateOptions Nothing Nothing Nothing Nothing

data DocumentMeta (k :: KeyType) = DocumentMeta
  { dmId  :: !ID
  , dmKey :: !(Key k)
  , dmRev :: !Rev
  }

instance A.FromJSON (DocumentMeta m) where
  parseJSON (A.Object o) =
    DocumentMeta <$> o A..: "_id" <*> o A..: "_key" <*> o A..: "_rev"
  parseJSON invalid = A.typeMismatch "DocumentMeta" invalid

data DocumentError
  = DocErrInvalidRequest !T.Text
  | DocErrUnknownCollection
  | DocErrConflict !T.Text
  | DocErrUnknown !HTTP.Status !T.Text
  | DocErrParseResponse !T.Text
  | DocErrOutOfKeys
  deriving Show

createDocuments
  :: A.ToJSON a
  => Collection k
  -> CreateOptions
  -> [a]
  -> IO (Either DocumentError [DocumentMeta k])
createDocuments _   _    []   = return (Right [])
createDocuments col opts docs = do
  res <- HTTP.httpLbs req (colManager col)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 || x == HTTP.status201 -> case dec of
      Right a   -> Right a
      Left  err -> Left $ DocErrParseResponse (T.pack err)
     where
      dec = case docs of
        [_] -> (: []) <$> A.eitherDecode (HTTP.responseBody res)
        _   -> A.eitherDecode (HTTP.responseBody res)
    x | x == HTTP.status400 -> Left (DocErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status404 -> Left DocErrUnknownCollection
    x | x == HTTP.status409 -> Left (DocErrConflict (readErrorMessage res))
    x                       -> Left (DocErrUnknown x (readErrorMessage res))
 where
  path = "/_api/document/" <> colName col <> "?" <> toQueryParams
    [ ("waitForSync=", Param $ coWaitForSync opts)
    , ("returnOld="  , Param $ coReturnOld opts)
    , ("silent="     , Param $ coSilent opts)
    , ("overwrite="  , Param $ coOverwrite opts)
    ]
  body = case docs of
    [x] -> A.encode x -- Allows ArrangoDb to optimize
    _   -> A.encode docs
  req = r
    { HTTP.method         = HTTP.methodPost
    , HTTP.requestBody    = HTTP.RequestBodyLBS (GZip.compress body)
    , HTTP.requestHeaders = gzipHeader : HTTP.requestHeaders r
    }
    where r = dbMkReq (colDb col) path

class BulkDocument a where
  bulkHeaders :: BulkHeader a
  toBulkFormat :: a -> BB.Builder

data BulkHeader a = BulkField BB.Builder | BulkFields [BulkHeader a]

data BulkImportResult = BulkImportResult
  { birCreated :: !Int
  , birErrors  :: !Int
  , birEmpty   :: !Int
  , birUpdated :: !Int
  , birIgnored :: !Int
  , birDetails :: ![T.Text]
  }

-- brittany-disable-next-binding
instance A.FromJSON BulkImportResult where
  parseJSON (A.Object r) =
    BulkImportResult
      <$>  r A..: "created"
      <*>  r A..: "errors"
      <*>  r A..: "empty"
      <*>  r A..: "updated"
      <*>  r A..: "ignored"
      <*>  (r A..:? "details" A..!= [])
  parseJSON invalid = A.typeMismatch "BulkImportResult" invalid

data BulkImportOptions = BulkImportOptions
  { bioFromPrefix  :: !(Maybe T.Text)
  , bioToPrefix    :: !(Maybe T.Text)
  , bioOverwrite   :: !(Maybe Bool)
  , bioOnDuplicate :: !(Maybe DuplicateAction)
  , bioComplete    :: !(Maybe Bool)
  , bioDetails     :: !(Maybe Bool)
  }

defBulkImportOptions :: BulkImportOptions
defBulkImportOptions = BulkImportOptions
  { bioFromPrefix  = Nothing
  , bioToPrefix    = Nothing
  , bioOverwrite   = Nothing
  , bioOnDuplicate = Nothing
  , bioComplete    = Nothing
  , bioDetails     = Nothing
  }

data DuplicateAction = DAError | DAUpdate | DAReplace | DAIgnore

instance ToParamValue DuplicateAction where
  toParamValue x = case x of
    DAError   -> "error"
    DAUpdate  -> "update"
    DAReplace -> "replace"
    DAIgnore  -> "ignore"

bulkImport
  :: forall a k
   . BulkDocument a
  => Collection k
  -> BulkImportOptions
  -> [a]
  -> IO (Either DocumentError BulkImportResult)
bulkImport col opts docs = do
  res <- HTTP.httpLbs req (colManager col)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status201 -> case A.eitherDecode (HTTP.responseBody res) of
      Right a   -> Right a
      Left  err -> Left $ DocErrParseResponse (T.pack err)
    x | x == HTTP.status400 -> Left (DocErrInvalidRequest (readErrorMessage res))
    x | x == HTTP.status404 -> Left DocErrUnknownCollection
    x | x == HTTP.status409 -> Left (DocErrConflict (readErrorMessage res))
    x | x == HTTP.status500 -> Left DocErrOutOfKeys
    x                       -> Left (DocErrUnknown x (readErrorMessage res))
 where
  params = toQueryParams
    [ ("fromPrefix=" , Param $ bioFromPrefix opts)
    , ("toPrefix="   , Param $ bioToPrefix opts)
    , ("overwrite="  , Param $ bioOverwrite opts)
    , ("onDuplicate=", Param $ bioOnDuplicate opts)
    , ("complete="   , Param $ bioComplete opts)
    , ("details="    , Param $ bioDetails opts)
    ]
  path =
    "/_api/import?collection="
      <> colName col
      <> (if BS.length params > 0 then "&" <> params else mempty)
  toField (BulkField  t ) = "\"" <> t <> "\""
  toField (BulkFields ts) = "[" <> (mconcat . intersperse "," $ fmap toField ts) <> "]"
  headers (BulkField t) = "[\"" <> t <> "\"]"
  headers t             = toField t
  body =
    GZip.compress
      .  BB.toLazyByteString
      $  headers (bulkHeaders :: BulkHeader a)
      <> "\n"
      <> foldl (\agg v -> agg <> toBulkFormat v <> "\n") "" docs
  req = r
    { HTTP.method         = HTTP.methodPost
    , HTTP.requestBody    = HTTP.RequestBodyLBS body
    , HTTP.requestHeaders = gzipHeader : HTTP.requestHeaders r
    }
    where r = dbMkReq (colDb col) path

data Edge = Edge
  { eTo   :: !ID
  , eFrom :: !ID
  }

instance A.ToJSON Edge where
  toJSON _ = error "ToJSON not support for edge"
  toEncoding e = A.pairs ("_to" A..= eTo e <> "_from" A..= eFrom e)
