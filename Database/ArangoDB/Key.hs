{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Database.ArangoDB.Key
  ( Key
  , KeyType(..)
  , toNumericKey
  , fromNumericKey
  , toUUIDKey
  , fromUUIDKey
  , toTextKey
  , fromTextKey
  , toPadded16Key
  , fromPadded16Key
  ) where

import Data.Text.Read (decimal)
import Data.Maybe (fromMaybe)

import TextShow (TextShow(..))

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.UUID as UUID

data KeyType = NumericKey | Padded16Key | UUIDKey | TextKey

instance A.ToJSON (Key k) where
  toJSON (Key k) = A.String k
  toEncoding (Key k) = A.toEncoding k

newtype Key (k ::KeyType) = Key T.Text

instance A.FromJSON (Key k) where
  parseJSON (A.String k) = pure (Key k)
  parseJSON invalid = A.typeMismatch "Key" invalid

toNumericKey :: (Integral a, TextShow a) => a -> Key 'NumericKey
toNumericKey = Key . showt

fromNumericKey :: Integral a => Key 'NumericKey -> a
fromNumericKey (Key k) = either error fst $ decimal k

toUUIDKey :: UUID.UUID -> Key 'UUIDKey
toUUIDKey = Key . UUID.toText

fromUUIDKey :: Key 'UUIDKey -> UUID.UUID
fromUUIDKey (Key k) = fromMaybe (error "Invalid UUID") (UUID.fromText k)

toPadded16Key :: T.Text -> Key 'Padded16Key
toPadded16Key t
  | T.length t == 16 = Key t
  | otherwise = error "Key value must be 16 bytes"

fromPadded16Key :: Key 'Padded16Key -> T.Text
fromPadded16Key (Key k) = k

toTextKey :: T.Text -> Key 'TextKey
toTextKey t
  | T.length t < 255 = Key t
  | otherwise = error "Key value must be less then 255 bytes"

fromTextKey :: Key 'TextKey -> T.Text
fromTextKey (Key k) = k
