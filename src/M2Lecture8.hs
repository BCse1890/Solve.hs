{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module M2Lecture8 where

import Control.Monad.Logger
import Data.Maybe (fromJust)
import qualified Data.Heap as H
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T

import M1Lecture10
import Utils

mergeSortedLists :: (Ord a) => [[a]] -> [a]
mergeSortedLists lists = undefined

closestKNumbers :: [Int] -> Int -> Int -> [Int]
closestKNumbers values comparison k = undefined

orderWaitTime :: (MonadLogger m) => [(Int,Int)] -> m Int
orderWaitTime customers = undefined

runningMedian :: [Double] -> [Double]
runningMedian numbers = undefined

buildHuffmanTree :: (MonadLogger m) => String -> m HuffmanTree
buildHuffmanTree input = undefined

prim :: [(String, String, Int)] -> (Int, [(String, String)])
prim inputEdges = undefined
