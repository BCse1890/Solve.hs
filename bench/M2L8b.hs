module Main where

import Control.Monad (replicateM)
import Criterion
import Criterion.Main (defaultMain)
import qualified Data.List as L
import System.Random

import qualified M2Lecture8 as L8

main :: IO ()
main = do
  l1 <- replicateM 4 (randomList 2500)
  l2 <- replicateM 4 (randomList 25000)
  l3 <- replicateM 32 (randomList 3125)
  l4 <- randomList 10000
  l5 <- randomList 100000
  defaultMain
    [ bgroup "Lists benchmark"
      [ bench "mergeSortedLists n=10000 k=4 Naive" $ nf mergeSortedListsNaive l1
      , bench "mergeSortedLists n=10000 k=4 Heap" $ nf L8.mergeSortedLists l1
      , bench "mergeSortedLists n=100000 k=4 Naive" $ nf mergeSortedListsNaive l2
      , bench "mergeSortedLists n=100000 k=4 Heap" $ nf L8.mergeSortedLists l2
      , bench "mergeSortedLists n=100000 k=32 Naive" $ nf mergeSortedListsNaive l3
      , bench "mergeSortedLists n=100000 k=32 Heap" $ nf L8.mergeSortedLists l3
      , bench "closestKNumber n=10000 k=4 Naive" $ nf (closestKNumbersSort l4 5000) 4
      , bench "closestKNumber n=10000 k=4 Heap" $ nf (L8.closestKNumbers l4 5000) 4
      , bench "closestKNumber n=100000 k=4 Naive" $ nf (closestKNumbersSort l5 5000) 4
      , bench "closestKNumber n=100000 k=4 Heap" $ nf (L8.closestKNumbers l5 5000) 4
      , bench "closestKNumber n=100000 k=32 Naive" $ nf (closestKNumbersSort l5 5000) 32
      , bench "closestKNumber n=100000 k=32 Heap" $ nf (L8.closestKNumbers l5 5000) 32
      ]
    ]

mergeSortedListsNaive :: (Ord a) => [[a]] -> [a]
mergeSortedListsNaive lists = L.sort (L.concat lists)

closestKNumbersSort :: [Int] -> Int -> Int -> [Int]
closestKNumbersSort numbers comparison k = if length numbers <= k
  then L.sort numbers
  else L.sort (snd <$> take k sorted)
  where
    tupled = map (\a -> (abs (comparison - a), a)) numbers
    sorted = L.sort tupled

-- Generate a list of a particular size
randomList :: Int -> IO [Int]
randomList n = replicateM n (randomRIO (1, 100000 :: Int))
