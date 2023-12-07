module Main where

import qualified Data.List as L
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import M2Lecture8
import Utils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 8 Tests"
  [ mergeSortedListsTests
  , closestKNumbersTests
  , orderWaitTimeTests
  , runningMedianTests
  , primTests
  , buildHuffmanTreeTests
  ]

mergeSortedListsTests :: TestTree
mergeSortedListsTests = testGroup "mergeSortedLists"
  [ testCase "mergeSortedLists 1" $ mergeSortedLists [] @?= ([] :: [String])
  , testCase "mergeSortedLists 2" $ mergeSortedLists [[5],[],[]] @?= [5]
  , testCase "mergeSortedLists 3" $ mergeSortedLists [[5],[],[4]] @?= [4,5]
  , testCase "mergeSortedLists 4" $ mergeSortedLists [[5,8,16,25],[],[4,8,12]] @?= [4,5,8,8,12,16,25]
  , testCase "mergeSortedLists 5" $ mergeSortedLists [[5],[3],[4]] @?= [3,4,5]
  , testCase "mergeSortedLists 6" $ mergeSortedLists
      [ [1,2,3,4,5,7,9]
      , [10,12,13,15,16,17,19]
      , [23,25,26,28,30]
      ] @?= [1,2,3,4,5,7,9,10,12,13,15,16,17,19,23,25,26,28,30]
  , testCase "mergeSortedLists 7" $ mergeSortedLists
      [ [64,89,135,141,152,177]
      , [1,6,15,45,72,201,265]
      , [-5,-3,0,89,90,150,199,299]
      ] @?= [-5,-3,0,1,6,15,45,64,72,89,89,90,135,141,150,152,177,199,201,265,299]
  , testCase "mergeSortedLists 8" $ mergeSortedLists
      [ L.sort [47,34,4,86,82,87,55,44,53,64]
      , L.sort [8,20,97,74,94,89,74,16,23,98]
      , L.sort [51,99,26,66,90,95,59,56,46,86]
      , L.sort [19,25,50,93,17,84,74,47,3,99]
      , L.sort [64,62,48,70,73,15,30,70,5,71]
      , L.sort [72,5,32,79,92,100,17,8,62,33]
      , L.sort [98,98,32,43,70,47,63,79,9,86]
      , L.sort [2,68,52,5,79,70,74,96,44,14]
      , L.sort [24,80,27,75,40,29,40,40,73,33]
      , L.sort [17,77,31,9,72,1,12,86,28,23]
      ] @?= [ 1,2,3,4,5,5,5,8,8,9,9,12,14,15,16,17,17,17,19,20,23,23,24,25,26,27,28,29,30,31,32
            , 32,33,33,34,40,40,40,43,44,44,46,47,47,47,48,50,51,52,53,55,56,59,62,62,63,64,64
            , 66,68,70,70,70,70,71,72,72,73,73,74,74,74,74,75,77,79,79,79,80,82,84,86,86,86,86
            , 87,89,90,92,93,94,95,96,97,98,98,98,99,99,100
            ]
  ]

closestKNumbersTests :: TestTree
closestKNumbersTests = testGroup "closestKNumbers"
  [ testCase "closestKNumbers 1" $ closestKNumbers [] 5 3 @?= []
  , testCase "closestKNumbers 2" $ closestKNumbers [10,20,-15,4] 5 6 @?= [-15,4,10,20]
  , testCase "closestKNumbers 3" $ closestKNumbers [10,20,-15,4] 5 1 @?= [4]
  , testCase "closestKNumbers 4" $ closestKNumbers [10,20,-15,4] 5 2 @?= [4,10]
  , testCase "closestKNumbers 5" $ closestKNumbers [10,20,-15,4] 5 3 @?= [4,10,20]
  , testCase "closestKNumbers 6" $ closestKNumbers
      [-5,-3,0,89,90,150,199,299] 121 3 @?= [89,90,150]
  , testCase "closestKNumbers 7" $ closestKNumbers
      [47,34,4,86,82,87,55,44,53,64,8,20,97,74,94,89,74,16,23,98] 75 6 @?=
        [64,74,74,82,86,87]
  , testCase "closestKNumbers 8" $ closestKNumbers
      [ 47,34,4,86,82,87,55,44,53,64
      , 8,20,97,74,94,89,74,16,23,98
      , 51,99,26,66,90,95,59,56,46,86
      , 19,25,50,93,17,84,74,47,3,99
      , 64,62,48,70,73,15,30,70,5,71
      , 72,5,32,79,92,100,17,8,62,33
      , 98,98,32,43,70,47,63,79,9,86
      , 2,68,52,5,79,70,74,96,44,14
      , 24,80,27,75,40,29,40,40,73,33
      , 17,77,31,9,72,1,12,86,28,23
      ] 50 15 @?= [43,44,44,46,47,47,47,48,50,51,52,53,55,56,59]
  ]

orderWaitTimeTests :: TestTree
orderWaitTimeTests = testGroup "orderWaitTime"
  [ testCase "orderWaitTime 1" $ shouldReturnLogger (orderWaitTime [(1,5)]) 5
  , testCase "orderWaitTime 2" $ shouldReturnLogger (orderWaitTime [(1,5),(2,6)]) 7
  , testCase "orderWaitTime 3" $ shouldReturnLogger (orderWaitTime [(0,10),(2,5),(4,3)]) 11
  , testCase "orderWaitTime 4" $ shouldReturnLogger (orderWaitTime [(0,2),(3,5),(4,3)]) 4
  , testCase "orderWaitTime 5" $ shouldReturnLogger (orderWaitTime
      [(0,2),(3,5),(4,3),(5,2),(12,6),(12,4),(26,5)]) 6
      -- 2    5      9     5     11      5     5
  , testCase "orderWaitTime 6" $ shouldReturnLogger (orderWaitTime
      [ (28,40),(20,36),(8,15),(39,40),(16,25),(28,19),(29,3),(40,28),(11,21)
      , (34,14),(13,24),(38,32),(40,15),(18,6),(34,40),(11,8),(23,35),(25,33)
      , (22,18),(21,13)
      ]) 197
  ]

runningMedianTests :: TestTree
runningMedianTests = testGroup "runningMedian"
  [ testCase "runningMedian 1" $ runningMedian [] `listApproxEqual` []
  , testCase "runningMedian 2" $ runningMedian [1.0] `listApproxEqual` [1.0]
  , testCase "runningMedian 3" $ runningMedian [5.5] `listApproxEqual` [5.5]
  , testCase "runningMedian 4" $ runningMedian [1.0, 2.0] `listApproxEqual` [1.0, 1.5]
  , testCase "runningMedian 5" $ runningMedian
      [1.0, 2.0, 5.0, 3.0, 0.0] `listApproxEqual` [1.0, 1.5, 2.0, 2.5, 2.0]
  , testCase "runningMedian 6" $ runningMedian
      [1.0, 11.0, 13.0, 12.0, 9.0, 15.0, 11.25, 0.0] `listApproxEqual`
      [1.0, 6.0, 11.0, 11.5, 11.0, 11.5, 11.25, 11.125]
  ]

primTests :: TestTree
primTests = testGroup "prim"
  [ testCase "prim 1" $ prim [] @?= (0, [])
  , testCase "prim 2" $ prim [("B", "A", 5)] @?= (5, [("A","B")])
  , testCase "prim 3" $ prim [("B", "A", 5), ("A", "C", 10), ("B","C",1)] @?=
      (6, [("A","B"), ("B", "C")])
  , testCase "prim 4" $ prim g1Edges @?=
      (42, [("A","B"),("A","C"),("C", "D"),("D","E")])
  , testCase "prim 5" $ prim g2Edges @?=
      (32, [("A","F"),("A","G"),("B","E"),("C","E"),("C","F"),("D","E")])
  , testCase "prim 6" $ prim g3Edges @?=
      (84, [("1","2"),("1","3"),("1","4"),("2","5"),("3","6"),("4","7")])
  , testCase "prim 7" $ prim g4Edges @?=
      (23, [("1","2"),("1","4"),("2","3")])
  ]

buildHuffmanTreeTests :: TestTree
buildHuffmanTreeTests = testGroup "buildHuffmanTree"
  [ testCase "buildHuffmanTree 1" $ shouldReturnLogger (buildHuffmanTree "aab")
      (HInternal (HLeaf 'b' 1) (HLeaf 'a' 2) 3)
  , testCase "buildHuffmanTree 2" $ shouldReturnLogger (buildHuffmanTree "cat")
      (HInternal (HLeaf 't' 1) (HInternal (HLeaf 'a' 1) (HLeaf 'c' 1) 2) 3)
  , testCase "buildHuffmanTree 3" $ shouldReturnLogger (buildHuffmanTree "a baked beet babble")
      (HInternal
        (HInternal (HLeaf 'e' 4)
          (HInternal
            (HInternal (HLeaf 'd' 1) (HLeaf 'k' 1) 2)
            (HInternal (HLeaf 'l' 1) (HLeaf 't' 1) 2)
            4)
          8)
        (HInternal (HLeaf 'b' 5)
          (HInternal (HLeaf ' ' 3) (HLeaf 'a' 3) 6) 11)
        19)
  , testCase "buildHuffmanTree 4" $ shouldReturnLogger (buildHuffmanTree
      "fafeefafeefeefafcefcefcfafddffdddffafffbbffbfffbfffccfdeedeeddfddeeddeefccfffbbbffffbbffccfcffccffff")
      (HInternal
        (HLeaf 'f' 45)
        (HInternal
          (HInternal
            (HLeaf 'c' 12)
            (HLeaf 'd' 13)
            25)
          (HInternal
            (HInternal (HLeaf 'a' 5) (HLeaf 'b' 9) 14)
            (HLeaf 'e' 16)
            30)
          55)
        100)
  ]
