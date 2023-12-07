{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Control.Monad (replicateM)
import Criterion
import Criterion.Main (defaultMain)
import System.Random


import qualified MySet as S
import qualified BalancedSet as B

main :: IO ()
main = do
  let l1 = [1..100 :: Int]
  let l2 = [1..1000 :: Int]
  let [l3, l4] = map reverse [l1, l2]
  [l5, l6, l7] <- mapM
    randomList [100, 1000, 10000]
  let [s1, s2, s3, s4, s5, s6, s7] = map S.fromList [l1, l2, l3, l4, l5, l6, l7]
  let [b1, b2, b3, b4, b5, b6, b7] = map B.fromList [l1, l2, l3, l4, l5, l6, l7]
  defaultMain
    [ bgroup "Lists benchmark"
      [ bench "Size 100 Ordered (MySet)" $ nf (map (`S.member` s1)) l1
      , bench "Size 100 Ordered (Balanced)" $ nf (map (`B.member` b1)) l1
      , bench "Size 1000 Ordered (MySet)" $ nf (map (`S.member` s2)) l2
      , bench "Size 1000 Ordered (Balanced)" $ nf (map (`B.member` b2)) l2
      , bench "Size 100 Reversed (MySet)" $ nf (map (`S.member` s3)) l3
      , bench "Size 100 Reversed (Balanced)" $ nf (map (`B.member` b3)) l3
      , bench "Size 1000 Reversed (MySet)" $ nf (map (`S.member` s4)) l4
      , bench "Size 1000 Reversed (Balanced)" $ nf (map (`B.member` b4)) l4
      , bench "Size 100 Random (MySet)" $ nf (map (`S.member` s5)) l5
      , bench "Size 100 Random (Balanced)" $ nf (map (`B.member` b5)) l5
      , bench "Size 1000 Random (MySet)" $ nf (map (`S.member` s6)) l6
      , bench "Size 1000 Random (Balanced)" $ nf (map (`B.member` b6)) l6
      , bench "Size 10000 Random (MySet)" $ nf (map (`S.member` s7)) l7
      , bench "Size 10000 Random (Balanced)" $ nf (map (`B.member` b7)) l7
      ]
    ]

-- Generate a list of a particular size
randomList :: Int -> IO [Int]
randomList n = replicateM n (randomRIO (1, 100000 :: Int))
