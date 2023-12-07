{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Control.Monad (replicateM)
import Criterion
import Criterion.Main (defaultMain)
import qualified Data.HashMap.Lazy as HM
import System.Random

import qualified HashTable as H

main :: IO ()
main = do
  [l1, l2, l3, l4] <- mapM
    randomList [100, 1000, 10000, 100000]
  [t1, t2, t3, t4] <- mapM H.fromList [l1, l2, l3, l4]
  let [hm1, hm2, hm3, hm4] = map HM.fromList [l1, l2, l3, l4]
  defaultMain
    [ bgroup "Lists benchmark"
      [ bench "Size 100 (HashTable)" $ nfIO (mapM (\(k,_) -> H.lookup k t1) l1)
      , bench "Size 1000 (HashTable)" $ nfIO (mapM (\(k,_) -> H.lookup k t2) l2)
      , bench "Size 10000 (HashTable)" $ nfIO (mapM (\(k,_) -> H.lookup k t3) l3)
      , bench "Size 100000 (HashTable)" $ nfIO (mapM (\(k,_) -> H.lookup k t4) l4)
      , bench "Size 100 (HashMap)" $ nf (map (\(k,_) -> HM.lookup k hm1)) l1
      , bench "Size 1000 (HashMap)" $ nf (map (\(k,_) -> HM.lookup k hm2)) l2
      , bench "Size 10000 (HashMap)" $ nf (map (\(k,_) -> HM.lookup k hm3)) l3
      , bench "Size 100000 (HashMap)" $ nf (map (\(k,_) -> HM.lookup k hm4)) l4
      ]
    ]

-- Generate a list of a particular size
randomList :: Int -> IO [(Int, Int)]
randomList n = do
  ns <- replicateM n (randomRIO (1, 100000 :: Int))
  mapM (\i -> return (i, i)) ns
