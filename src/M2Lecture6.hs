{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module M2Lecture6 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Array as A
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import qualified Data.HashSet as HS
import System.Random

import Utils

applyDenseMatrixOp :: (MArray array Double m) =>
  (Double -> Double -> Double) -> array (Int, Int) Double -> array (Int, Int) Double -> m ()
applyDenseMatrixOp f arr1 arr2 = undefined

intervalAddition :: [((Int, Int), Int)] -> Int
intervalAddition queries = undefined

insertionSort :: (Ord elem, MArray array elem m) => array Int elem -> m ()
insertionSort arr = undefined

quicksort :: (Ord elem, Show elem, MArray array elem m) => StdGen -> array Int elem -> m ()
quicksort gen arr = undefined

swap :: (MArray array elem m) => array Int elem -> Int -> Int -> m ()
swap arr i j = undefined

partition :: (Ord elem, MArray array elem m) =>
  array Int elem -> Int -> Int -> StdGen -> m (Int, StdGen)
partition arr start end gen1 = undefined

evenOdds :: IOArray Int Int -> IO ()
evenOdds arr = undefined

type Strike = (Coord2, Int, Direction8)

powerStrikes :: [Strike] -> IOArray Coord2 Int -> IO Int
powerStrikes strikes arr = undefined
