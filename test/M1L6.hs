module Main where

import Data.Char (toLower, toUpper)
import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture6
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 6 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ foldlTests
  , foldrTests
  , scanlTests
  , sumTests
  , mapTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ volumeAndSurfaceAreaTests
  , countMostAdjacentTests
  , robotHitsTests
  , math2dTests
  , seamCarvingTests
  , manhattanTravel2dTests
  ]

foldlTests :: TestTree
foldlTests = testGroup "foldl"
  [ testCase "foldl 1" $ foldl'' (+) 0 Nil @?= (0 :: Int)
  , testCase "foldl 2" $ foldl'' (+) 5 single @?= 6
  , testCase "foldl 3" $ foldl'' (*) 10 double @?= 20
  , testCase "foldl 4" $ foldl'' (*) 20 triple @?= 120
  , testCase "foldl 5" $ foldl'' (-) 20 triple @?= 14
  , testCase "foldl 6" $ foldl'' (++) "" many @?= "HelloWorld!Goodbye"
  ]

foldrTests :: TestTree
foldrTests = testGroup "foldr"
  [ testCase "foldr 1" $ foldr' (+) 0 Nil @?= (0 :: Int)
  , testCase "foldr 2" $ foldr' (+) 5 single @?= 6
  , testCase "foldr 3" $ foldr' (*) 10 double @?= 20
  , testCase "foldr 4" $ foldr' (*) 20 triple @?= 120
  , testCase "foldr 5" $ foldr' (-) 20 triple @?= -18
  , testCase "foldr 6" $ foldr' (++) "" many @?= "HelloWorld!Goodbye"
  ]

scanlTests :: TestTree
scanlTests = testGroup "scanl"
  [ testCase "scanl 1" $ scanl' (+) 0 Nil @?= Cons 0 Nil
  , testCase "scanl 2" $ scanl' (+) 5 single @?= mml [5, 6]
  , testCase "scanl 3" $ scanl' (*) 10 double @?= mml [10, 10, 20]
  , testCase "scanl 4" $ scanl' (*) 20 triple @?= mml [20, 20, 40, 120]
  , testCase "scanl 5" $ scanl' quot 20 triple @?= mml [20, 20, 10, 3]
  , testCase "scanl 6" $ scanl' (++) "" many @?=
      mml ["", "Hello", "HelloWorld", "HelloWorld!", "HelloWorld!Goodbye"]
  ]

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "sum 1" $ sum''' Nil @?= 0
  , testCase "sum 2" $ sum''' single @?= 1
  , testCase "sum 3" $ sum''' double @?= 3
  , testCase "sum 4" $ sum''' triple @?= 6
  , testCase "sum 5" $ sum''' (mml [2, 6, 9, 15]) @?= 32
  ]

mapTests :: TestTree
mapTests = testGroup "map"
  [ testCase "map 1" $ map'' (*2) Nil @?= Nil
  , testCase "map 2" $ map'' (*2) single @?= Cons 2 Nil
  , testCase "map 3" $ map'' (*2) double @?= mml [2, 4]
  , testCase "map 4" $ map'' (*2) triple @?= mml [2, 4, 6]
  , testCase "map 5" $ map'' show triple @?= mml ["1", "2", "3"]
  , testCase "map 6" $ map'' head many @?= mml ['H', 'W', '!', 'G']
  ]

volumeAndSurfaceAreaTests :: TestTree
volumeAndSurfaceAreaTests = testGroup "volumeAndSurfaceArea"
  [ testCase "volumeAndSurfaceArea 1" $ volumeAndSurfaceArea [] @?= (0, 0)
  , testCase "volumeAndSurfaceArea 2" $ volumeAndSurfaceArea [(2,2,2)] @?= (8, 24)
  , testCase "volumeAndSurfaceArea 3" $ volumeAndSurfaceArea [(2,2,2), (3, 1, 4), (5, 5, 8), (10, 13, 2)] @?= (480, 624)
  ]

countMostAdjacentTests :: TestTree
countMostAdjacentTests = testGroup "countMostAdjacent"
  [ testCase "countMostAdjacent 1" $ countMostAdjacent [] @?= 0
  , testCase "countMostAdjacent 2" $ countMostAdjacent [(1,4), (2,3), (3,5), (4,2)] @?= 8
  , testCase "countMostAdjacent 3" $ countMostAdjacent [(1,4), (2,3), (4,5), (5,2)] @?= 7
  , testCase "countMostAdjacent 4" $ countMostAdjacent [(5,2), (1,4), (2,3), (4,5)] @?= 7
  , testCase "countMostAdjacent 5" $ countMostAdjacent [(1,4), (2,3), (4,5), (5,2), (9, 10)] @?= 10
  , testCase "countMostAdjacent 6" $ countMostAdjacent [(5, 10)] @?= 10
  , testCase "countMostAdjacent 7" $ countMostAdjacent [(1, 3), (2, 4), (5, 10)] @?= 10
  ]

robotHitsTests :: TestTree
robotHitsTests = testGroup "robotHits"
  [ testCase "robotHits 1" $ robotHits 2 10 [] @?= (0, 0, 0)
  , testCase "robotHits 2" $ robotHits 2 10 [4] @?= (1, 0, 0)
  , testCase "robotHits 3" $ robotHits 2 10 [8] @?= (0, 1, 0)
  , testCase "robotHits 4" $ robotHits 2 10 [6] @?= (0, 0, 1)
  , testCase "robotHits 5" $ robotHits 2 10 [9, 10, 8, 7, 4] @?= (1, 3, 1)
  , testCase "robotHits 6" $ robotHits 1 3 [0, 0, 0, 0, 0, 1, 2, 2, 2, 2] @?= (5, 4, 1)
  , testCase "robotHits 7" $ robotHits (-3) 3 [0, 0, 0] @?= (0, 0, 3)
  ]

math2dTests :: TestTree
math2dTests = testGroup "math2d"
  [ testCase "math2d 1" $ math2d [] @?= 0
  , testCase "math2d 2" $ math2d [[1, 2, 3]] @?= 5
  , testCase "math2d 3" $ math2d [[1, 2, 3, 4]] @?= 20
  , testCase "math2d 4" $ math2d [[1, 2, 3, 4, 5]] @?= 25
  , testCase "math2d 5" $ math2d [[1, -2, 3, 4, -5]] @?= -1
  , testCase "math2d 6" $ math2d [[1, 2, 3, 4],[5, 6, 7, 8]] @?= 1256
  , testCase "math2d 7" $ math2d [[1, 2, 3, 4, 5],[5, 6, 7, 8, 9]] @?= 1505
  , testCase "math2d 8" $ math2d [[1, 2, 3], [-4, 5, 3], [2, 2, 2]] @?= 22
  , testCase "math2d 9" $ math2d [[4,3,-2], [6,2,13]] @?= 45
  ]

seamCarvingTests :: TestTree
seamCarvingTests = testGroup "seamCarving"
  [ testCase "seamCarving 1" $ seamCarving [[2, 1, 3, 4, 6,9,3]] @?= 1
  , testCase "seamCarving 2" $ seamCarving [[2, 1, 3, 4, 6], [8, 10, 5, 2, 1]] @?= 5
  , testCase "seamCarving 3" $ seamCarving
      [[1,4,3,5,2],[3,2,5,2,3],[5,2,4,2,1]] @?= 5
  , testCase "seamCarving 4" $ seamCarving
      [[17, 20, 35, 64, 23, 18], [42, 16, 55, 84, 42, 27], [33, 14, 56, 99, 79, 32]]
      @?= 47
  , testCase "seamCarving 5" $ seamCarving
      [[17, 20, 35, 64, 23, 18], [42, 16, 55, 84, 42, 27], [84, 97, 56, 99, 79, 32]]
      @?= 77
  ]

manhattanTravel2dTests :: TestTree
manhattanTravel2dTests = testGroup "manhattanTravel2d"
  [ testCase "manhattanTravel2d 1" $ manhattanTravel2d [] 0.5 @?~= (0,0)
  , testCase "manhattanTravel2d 2" $ manhattanTravel2d [(1,1)] 0.5 @?~= (1,1)
  , testCase "manhattanTravel2d 3" $ manhattanTravel2d [(1,1), (3,3)] 0.0 @?~= (1,1)
  , testCase "manhattanTravel2d 4" $ manhattanTravel2d [(1,1), (3,3)] 1.0 @?~= (3,3)
  , testCase "manhattanTravel2d 5" $ manhattanTravel2d [(1,1), (3,3)] 0.5 @?~= (1,3)
  , testCase "manhattanTravel2d 6" $ manhattanTravel2d [(1,1), (3,3)] 0.3 @?~= (1,2.2)
  , testCase "manhattanTravel2d 7" $ manhattanTravel2d path1 0.5 @?~= (2,4)
  , testCase "manhattanTravel2d 8" $ manhattanTravel2d path1 0.75 @?~= (3,2)
  , testCase "manhattanTravel2d 9" $ manhattanTravel2d path2 0.75 @?~= (6,-1)
  , testCase "manhattanTravel2d 10" $ manhattanTravel2d path2 0.95 @?~= (4,-1)
  , testCase "manhattanTravel2d 11" $ manhattanTravel2d path2 0.975 @?~= (3.5,-1)
  ]
  where
    path1 = [(0,0), (3,4), (6,2)]
    path2 = [(0,0), (3,4), (6,2), (5,-2), (3,-1)]
