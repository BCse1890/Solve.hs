module HashTable where

import Control.Monad (foldM)
import Data.Array.IO
import Data.List (find)
import Data.Hashable

data HashTable k v = HashTable

maxUtilization :: Double
maxUtilization = 0.7

empty :: IO (HashTable k v)
empty = undefined

singleton :: (Hashable k) => k -> v -> IO (HashTable k v)
singleton key value = undefined

null :: HashTable k v -> Bool
null ht = undefined

size :: HashTable k v -> Int
size ht = undefined

fromList :: (Hashable k) => [(k, v)] -> IO (HashTable k v)
fromList items = undefined

toList :: HashTable k v -> IO [(k, v)]
toList ht = undefined

insert :: (Hashable k) => k -> v -> HashTable k v -> IO (HashTable k v)
insert key value ht = undefined

delete :: (Hashable k) => k -> HashTable k v -> IO (HashTable k v)
delete key ht = undefined

lookup :: (Hashable k) => k -> HashTable k v -> IO (Maybe v)
lookup key ht = undefined

getUtilization :: HashTable k v -> IO Double
getUtilization ht = undefined
