{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Database.ArangoDB.Document where

import Database.ArangoDB.Key
import Database.ArangoDB.Types
import Database.ArangoDB.Internal

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
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

data DocumentMeta (k :: KeyType) = DocumentMeta
  { dmId  :: !T.Text
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
  print (HTTP.responseBody res)
  return $ case HTTP.responseStatus res of
    x | x == HTTP.status200 -> case dec of
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
  toBS True  = "true"
  toBS False = "false"
  path =
    "/_api/document/"
      <> colName col
      <> "?"
      <> ( BS.intercalate "&"
         . filter (/= mempty)
         $ [ maybe mempty (mappend "waitForSync=" . toBS) (coWaitForSync opts)
           , maybe mempty (mappend "returnOld=" . toBS)   (coReturnOld opts)
           , maybe mempty (mappend "silent=" . toBS)      (coSilent opts)
           , maybe mempty (mappend "overwrite=" . toBS)   (coOverwrite opts)
           ]
         )
  body = case docs of
    [x] -> A.encode x -- Allows ArrangoDb to optimize
    _   -> A.encode docs
  req = (dbMkReq (colDb col) path)
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS body
    }
