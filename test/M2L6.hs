{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (foldM, void)
import Data.Array.IO
import Data.List (sort)
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import Test.Tasty
import Test.Tasty.HUnit

import qualified HashTable as H
import M2Lecture6
import Utils
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 6 Tests"
  [ hashTableTests
  , practiceProblemTests
  ]

hashTableTests :: TestTree
hashTableTests = testGroup "HashTable Tests"
  [ nullTests
  , sizeTests
  , toListTests
  , lookupTests
  , utilizationTests
  ]

practiceProblemTests :: TestTree
practiceProblemTests = testGroup "Practice Problem Tests"
  [ denseMatrixOpTests
  , intervalAdditionTests
  , insertionSortTests
  , quicksortTests
  , evenOddsTests
  , powerStrikesTests
  ]

(@@?=) :: (Eq a, Show a) => IO a -> a -> Assertion
(@@?=) action expectedResult = do
  actualResult <- action
  actualResult @?= expectedResult

nullTests :: TestTree
nullTests = testGroup "null"
  [ testCase "null 1" $ (H.null <$> H.empty) @@?= True
  , testCase "null 2" $ (H.null <$> H.singleton "Hi" 1) @@?= False
  , testCase "null 3" $ (H.null <$> H.fromList [("Hi", 1), ("Bye", 2)]) @@?= False
  , testCase "null 4" $ do
      h1 <- H.empty
      h2 <- H.insert "Hi" 1 h1
      H.null h2 @?= False
  , testCase "null 5" $ do
      h1 <- H.singleton "Hi" 1
      h2 <- H.delete "Hi" h1
      H.null h2 @?= True
  ]

sizeTests :: TestTree
sizeTests = testGroup "size"
  [ testCase "size 1" $ (H.size <$> H.empty) @@?= 0
  , testCase "size 2" $ (H.size <$> (H.singleton "Hi" 1)) @@?= 1
  , testCase "size 3" $ (H.size <$> (H.fromList [("Hi", 1), ("Bye", 2), ("Hi", 5), ("World", 4)])) @@?= 3
  , testCase "size 4" $ do
      h1 <- H.empty
      h2 <- H.insert "Hi" 1 h1
      H.size h2 @?= 1
  , testCase "size 5" $ do
      h1 <- H.singleton "Hi" 1
      h2 <- H.insert "Hi" 3 h1
      H.size h2 @?= 1
  , testCase "size 6" $ do
      h1 <- H.singleton "Hi" 1
      h2 <- H.delete "Hi" h1
      H.size h2 @?= 0
  , testCase "size 7" $ do
      h1 <- H.fromList [(1 :: Int, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)]
      h2 <- H.insert 7 7 h1
      h3 <- H.insert 8 8 h2
      h4 <- H.insert 9 9 h3
      H.size h4 @?= 9
  ]

toListTests :: TestTree
toListTests = testGroup "toList"
  [ testCase "toList 1" $ (H.empty >>= H.toList) @@?= ([] :: [(Int, Int)])
  , testCase "toList 2" $ ((H.singleton "Hi" 1) >>= H.toList) @@?= [("Hi", 1)]
  , testCase "toList 3" $ do
      h1 <- H.fromList [("Hi", 1), ("Bye", 2), ("Hi", 5), ("Good", 5)]
      (sort <$> H.toList h1) @@?= [("Bye", 2), ("Good", 5), ("Hi", 5)]
  , testCase "toList 4" $ do
      h1 <- H.singleton "Hi" 5
      h2 <- H.insert "Bye" 3 h1
      (sort <$> H.toList h2) @@?= [("Bye", 3), ("Hi", 5)]
  , testCase "toList 5" $ do
      h1 <- H.singleton "Hi" 5
      h2 <- H.delete "Hi" h1
      H.toList h2 @@?= []
  , testCase "toList 6" $ do
      h1 <- (H.fromList [(3, 3), (9, 9), (5, 5), (1, 1), (6, 6)])
      h2 <- H.insert 10 10 h1
      h3 <- H.insert 2 2 h2
      h4 <- H.insert 4 4 h3
      h5 <- H.insert 7 7 h4
      h6 <- H.insert 8 8 h5
      (sort <$> H.toList h6) @@?=
        [(1 :: Int, 1 :: Int), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (9,9), (10, 10)]
  ]

lookupTests :: TestTree
lookupTests = testGroup "lookup"
  [ testCase "lookup 1" $ (H.empty >>= H.lookup (1 :: Int)) @@?= (Nothing :: Maybe Int)
  , testCase "lookup 2" $ (H.singleton "Hi" 1 >>= H.lookup "Hi") @@?= Just 1
  , testCase "lookup 3" $ (H.singleton "Hi" 1 >>= H.lookup "Bye") @@?= Nothing
  , testCase "lookup 4" $
      ((H.fromList [(1 :: Int, 1), (2, 2), (5,3), (5,5)]) >>= H.lookup 2) @@?= Just 2
  , testCase "lookup 5" $
      ((H.fromList [(1 :: Int, 1), (2, 2), (5,3), (5,5)]) >>= H.lookup 5) @@?= Just 5
  , testCase "lookup 6" $
      ((H.fromList [(1 :: Int, 1), (2, 2), (5,3), (5,5)]) >>= H.lookup 6) @@?= Nothing
  , testCase "lookup 7" $ do
      h1 <- (H.fromList [(1 :: Int, 1), (2, 2), (5,3), (5,5)])
      h2 <- H.delete 5 h1
      H.lookup 5 h2 @@?= Nothing
  , testCase "lookup 8" $ do
      h1 <- H.singleton (8 :: Int) (8 :: Int)
      h2 <- H.delete 8 h1
      H.lookup 8 h2 @@?= Nothing
  ]

utilizationTests :: TestTree
utilizationTests = testGroup "utilization"
  [ testCase "utilization 1" $ do
      h1 <- H.empty
      h2 <- foldM (\h (k, v) -> H.insert k v h) h1 [(i, i) | i <- [1..9 :: Int]]
      u <- H.getUtilization h2
      assertBool "Exceeded max utilization!" (u <= H.maxUtilization)
  , testCase "utilization 2" $ do
      h1 <- H.empty
      h2 <- foldM (\h (k, v) -> H.insert k v h) h1 [(i, i) | i <- [1..100 :: Int]]
      u <- H.getUtilization h2
      assertBool "Exceeded max utilization!" (u <= H.maxUtilization)
  ]

denseMatrixOpTests :: TestTree
denseMatrixOpTests = testGroup "denseMatrixOp"
  [ testCase "denseMatrixOp 1" $ testMatrixOp (+) e22 m1 [1,1,0,0]
  , testCase "denseMatrixOp 2" $ testMatrixOp (-) e22 m1 [-1,-1,0,0]
  , testCase "denseMatrixOp 3" $ testMatrixOp (+) m1 m2 [2,1,1,0]
  , testCase "denseMatrixOp 4" $ testMatrixOp (*) m1 m2 [1,0,0,0]
  , testCase "denseMatrixOp 5" $ testMatrixOp (+) m3 m4
      [1,4,0,0,0,9,0,0,0,0,1,12,0,0,0,3]
  , testCase "denseMatrixOp 6" $ testMatrixOp (*) m3 m4
      [0,0,0,0,0,14,0,0,0,0,0,0,0,0,0,0]
  , testCase "denseMatrixOp 7" $ testMatrixOp (-) m3 m4
      [1,-4,0,0,0,5,0,0,0,0,-1,12,0,0,0,-3]
  , testCase "denseMatrixOp 8" $ testMatrixOp (/) m5 m6
      [1.0,0.333333,2.0,0.0,0.0,0.75]
  ]
  where
    e22 :: IO (IOArray (Int, Int) Double)
    e22 = newArray ((0,0),(1,1)) 0

    m1 :: IO (IOArray (Int, Int) Double)
    m1 = newListArray ((0,0),(1,1)) [1,1,0,0]

    m2 :: IO (IOArray (Int, Int) Double)
    m2 = newListArray ((0,0),(1,1)) [1,0,1,0]

    m3 :: IO (IOArray (Int, Int) Double)
    m3 = newListArray ((0,0),(3,3)) [1,0,0,0,0,7,0,0,0,0,0,12,0,0,0,0]

    m4 :: IO (IOArray (Int, Int) Double)
    m4 = newListArray ((0,0),(3,3)) [0,4,0,0,0,2,0,0,0,0,1,0,0,0,0,3]

    m5 :: IO (IOArray (Int, Int) Double)
    m5 = newListArray ((0,0),(1,2)) [1,2,4,0,0,6]

    m6 :: IO (IOArray (Int, Int) Double)
    m6 = newListArray ((0,0),(1,2)) [1,6,2,5,4,8]

    testMatrixOp :: (Double -> Double -> Double)
      -> IO (IOArray (Int, Int) Double)
      -> IO (IOArray (Int, Int) Double)
      -> [Double]
      -> Assertion
    testMatrixOp f arrAction1 arrAction2 expectedElems = do
      arr1 <- arrAction1
      arr2 <- arrAction2
      applyDenseMatrixOp f arr1 arr2
      actualElems <- getElems arr1
      expectedElems `listApproxEqual` actualElems

intervalAdditionTests :: TestTree
intervalAdditionTests = testGroup "intervalAddition"
  [ testCase "intervalAddition 1" $ intervalAddition [((1,3),4)] @?= 4
  , testCase "intervalAddition 2" $ intervalAddition [((1,1),5)] @?= 5
  , testCase "intervalAddition 3" $ intervalAddition [((1,1),5),((2,2),6)] @?= 6
  , testCase "intervalAddition 4" $ intervalAddition [((1,4),5),((2,6),6)] @?= 11
  , testCase "intervalAddition 5" $ intervalAddition
      [((1,100),1),((2,10),2),((90,99),3),((10,90),4)] @?= 8
  , testCase "intervalAddition 6" $ intervalAddition
      [((1,3),6),((3,5),4),((6,10),7),((4,7),15)] @?= 22
  , testCase "intervalAddition 7" $ intervalAddition
      [ ((56, 74), 13)
      , ((19, 98), 34)
      , ((21, 52), 19)
      , ((47, 90),  2)
      , ((21, 85), 21)
      , ((66, 69), 97)
      , ((15, 83), 70)
      , ((19, 98), 68)
      , (( 3, 50), 88)
      , ((43, 99), 94)
      , ((13, 83), 72)
      , ((22, 75), 45)
      ] @?= 516
  ]

testSortArray :: (Ord a, Show a) => (IOArray Int a -> IO ()) -> [a] -> Assertion
testSortArray sortF items = do
  arr <- newListArray (0, length items - 1) items
  sortF arr
  final <- getElems arr
  sort items @?= final

insertionSortTests :: TestTree
insertionSortTests = testGroup "insertionSort"
  [ testCase "insertionSort 1" $ testSortArray insertionSort ([] :: [Int])
  , testCase "insertionSort 2" $ testSortArray insertionSort [1]
  , testCase "insertionSort 3" $ testSortArray insertionSort [4, 1, 3, 2, 0]
  , testCase "insertionSort 4" $ testSortArray insertionSort [6,8,1,5,4,7,11,10,2,3,9]
  , testCase "insertionSort 5" $ testSortArray insertionSort ["Hi", "Bye", "Good"]
  ]

quicksortTests :: TestTree
quicksortTests = testGroup "quicksort"
  [ testCase "quicksort 1" $ tq ([] :: [Int])
  , testCase "quicksort 2" $ tq [1]
  , testCase "quicksort 3" $ tq [4, 1, 3, 2, 0]
  , testCase "quicksort 4" $ tq [6,8,1,5,4,7,11,10,2,3,9]
  , testCase "quicksort 5" $ tq ["Hi", "Bye", "Good"]
  ]
  where
    tq es = do
      g <- getStdGen
      testSortArray (quicksort g) es

evenOddsTests :: TestTree
evenOddsTests = testGroup "evenOdds"
  [ testCase "evenOdds 1" $ tEO ((0,0), [2]) [2]
  , testCase "evenOdds 2" $ tEO ((0,0), [3]) [3]
  , testCase "evenOdds 3" $ tEO ((0,1), [2,3]) [3,2]
  , testCase "evenOdds 4" $ tEO ((0,9), [1,2,3,4,5,6,7,8,9,10]) [1,2,3,4,5,6,7,8,9,10]
  , testCase "evenOdds 5" $ tEO ((0,9), [0,1,2,3,4,5,6,7,8,9]) [1,0,3,2,5,4,7,6,9,8]
  , testCase "evenOdds 6" $ tEO ((1,10), [0,1,2,3,4,5,6,7,8,9]) [1,0,3,2,5,4,7,6,9,8]
  , testCase "evenOdds 7" $ tEO ((2,10), [2,3,2,4,7,6,9,10,12]) [3,2,7,2,9,4,6,10,12]
  , testCase "evenOdds 8" $ tEO ((3,12), [2,3,2,4,7,6,9,13,11,1]) [3,2,7,2,9,4,13,6,11,1]
  , testCase "evenOdds 9" $ tEO ((1,30),
      [85,67,67,74,60,48,64,31,11,16,88,87,67,68,42,19,98,14,93,12,78,2,35,79,43,33,99,32,28,73])
      [85,74,67,60,67,48,31,64,11,16,87,88,67,68,19,42,93,98,35,14,79,12,43,78,33,2,99,32,73,28]
  , testCase "evenOdds 10" $ tEO ((1,30),
      [86,68,68,75,61,49,63,32,10,17,89,88,66,69,43,20,99,15,94,13,77,3,36,78,44,32,98,31,29,74])
      [75,86,61,68,49,68,63,32,17,10,89,88,69,66,43,20,99,94,15,36,13,78,77,44,3,32,31,98,29,74]
  ]
  where
    tEO (is, elems) expectedOutputs = do
      arr <- (newListArray is elems :: IO (IOArray Int Int))
      evenOdds arr
      actualOutputs <- getElems arr
      expectedOutputs @?= actualOutputs

powerStrikesTests :: TestTree
powerStrikesTests = testGroup "powerStrikes"
  [ testCase "powerStrikes 1" $ testPS (10,10) [((3,4),5,Right8)] 0 ((3,4),5)
  , testCase "powerStrikes 2" $ testPS (10, 10)
      [((3,4),5,Right8),((3,3),9,UpRight)] 1 ((4,4),0)
  , testCase "powerStrikes 3" $ testPS (10, 10)
      [((3,4),5,Right8),((4,8),9,Up8),((3,3),9,UpRight)] 2 ((3,9),3)
  , testCase "powerStrikes 4" $ testPS (10, 10)
      (circleStrikes <> [((6,0),1,Right8)]) 8 ((6,0),0)
  , testCase "powerStrikes 5" $ testPS (10, 10)
      (circleStrikes <> [((6,0),1,Left8)]) 1 ((3,0),9)
  , testCase "powerStrikes 6" $ testPS (10, 10)
      (circleStrikes <> [((6,0),1,UpLeft)]) 8 ((1,2),2)
  , testCase "powerStrikes 7" $ testPS (10, 10)
      (circleStrikes <> [((6,0),1,UpLeft),((7,1),10,DownLeft)]) 9 ((6,0),9)
  , testCase "powerStrikes 8" $ testPS (10, 10)
      [ ((3,1),9,Left8), ((3,7),9,Left8),((6,1),9,Right8),((6,7),9,Right8)
      , ((9,4),6,Right8), ((9,1),3,Right8), ((9,7),3,Right8)
      ] 0 ((9,4),6)
  , testCase "powerStrikes 9" $ testPS (10, 10)
      [ ((3,1),9,Left8), ((3,7),9,Left8),((6,1),9,Right8),((6,7),9,Right8)
      , ((9,4),7,Right8), ((9,1),3,Right8), ((9,7),3,Right8)
      , ((0,4),10,Right8)
      ] 7 ((9,1),9)
  , testCase "powerStrikes 10" $ testPS (10, 10)
      [ ((3,1),9,Left8), ((3,7),9,Left8),((6,1),9,Right8),((6,7),9,Right8)
      , ((9,4),8,Right8), ((9,1),3,Right8), ((9,7),3,Right8)
      , ((0,4),10,Right8)
      ] 7 ((9,7),9)
  , testCase "powerStrikes 11" $ testPS (25,25)
      [ ((12,12), 14, UpLeft), ((16,14), 7, Left8), ((8,9),8,Right8),((10,20),10,DownLeft)
      , ((5,24),4,UpLeft), ((0,13),24,Right8), ((4,23),13,Down8), ((12,7),20,Up8)
      , ((8,22),12,UpLeft),((16,11),10,UpLeft), ((2,20),11,UpLeft), ((24,19),12,Left8)
      , ((5,17),10,Left8), ((0,12),12,Left8), ((15,8),11,Right8), ((14,2),15,Down8)
      , ((18,19),1,Right8), ((23,15),4,Right8), ((9,7),4,Up8), ((4,16),9,Up8), ((18,21),6,Down8)
      , ((6,20),11,Up8), ((7,11),19,DownRight), ((2,23),20,UpLeft), ((0,17),21,Up8)
      , ((12,5),12,Up8), ((13,14),17,DownRight)
      ] 1055 ((12,12), 5)
  ]
  where
    testPS (xs, ys) strikes expectedResult (testC, testV) = do
      (arr :: IOArray (Int, Int) Int) <- newArray ((0,0), (xs - 1, ys - 1)) 0
      actualResult <- powerStrikes strikes arr
      actualResult @?= expectedResult
      actualV <- readArray arr testC
      actualV @?= testV

    circleStrikes =
      [ ((6,0),9,Down8), ((9,3),9,Right8), ((9,6),9,Right8), ((6,9), 9, Up8)
      , ((3,9),9,Up8), ((0,6),9,Left8), ((0,3),9,Left8), ((3,0),9,Down8)
      ]
