{-# LANGUAGE ScopedTypeVariables #-}

module M2Lecture5 where

import Control.Monad.Logger
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Ix as I
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Vector as V

import Utils

binarySearch :: (I.Ix i, Ord a) =>
  (i -> container a -> a) -> -- Indexing Function
  (i, i) ->                  -- Range
  container a ->             -- Container
  a ->                       -- Desired Element
  Maybe i                    -- Result Index
binarySearch indexF rng@(start,end) vals comp = undefined

binarySearchV :: (Ord a) => V.Vector a -> a -> Maybe Int
binarySearchV vals = undefined

binarySearchA :: (Ord a, I.Ix i) => A.Array i a -> a -> Maybe i
binarySearchA vals = undefined

addArrays :: (MonadLogger m) => [A.Array Int Int] -> m (A.Array Int Int)
addArrays arrays = undefined

treeVisibilityRedux :: (MonadLogger m) => (Coord2, Coord2, Coord2) -> A.Array Coord2 Int -> m Int
treeVisibilityRedux (l1, l2, l3) trees = undefined

functionPermutation :: V.Vector Int -> V.Vector Int
functionPermutation v = undefined

boggleSearch :: (MonadLogger m) => A.Array (Int, Int) Char -> String -> m Bool
boggleSearch grid word = undefined

bingoBoard :: (MonadLogger m) => [Int] -> [Int] -> m Bool
bingoBoard board numbers = undefined
