module Database.ArangoDB.Collection where

import Database.ArangoDB.Internal

data Collection

data CollectionError

data CollectionConfig

createCollection :: Database -> CollectionConfig -> IO (Either CollectionError Collection)
createCollection = undefined
