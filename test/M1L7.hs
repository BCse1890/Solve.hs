module Main where

import Data.Char (toLower, toUpper)
import Test.Tasty
import Test.Tasty.HUnit

import M1Lecture7
import MyList
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 7 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ maximumByTests
  , sortTests
  , sortOnTests
  , sortByTests
  , groupTests
  , groupByTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ studentAwardsTests
  , anyOverlapTests
  , buildIntervalsTests
  , anagramsTests
  , buildMazeTests
  , incrementingChunksTests
  ]

maximumByTests :: TestTree
maximumByTests = testGroup "maximumBy"
  [ testCase "maximumBy 1" $ maximumBy' f single @?= 1
  , testCase "maximumBy 2" $ maximumBy' f double @?= 2
  , testCase "maximumBy 3" $ maximumBy' f triple @?= 2
  , testCase "maximumBy 4" $ maximumBy' f (mml [1, 3, 5, 6, 8, 9, 11]) @?= 6
  , testCase "maximumBy 5" $ maximumBy' g many @?= "!"
  ]
  where
    f x y = case (even x, even y) of
      (False, True) -> LT
      (True, False) -> GT
      _ -> y `compare` x
    g s1 s2 = last s2 `compare` last s1

sortTests :: TestTree
sortTests = testGroup "sort"
  [ testCase "sort 1" $ sort' Nil @?= (Nil :: MyList Int)
  , testCase "sort 2" $ sort' single @?= single
  , testCase "sort 3" $ sort' triple @?= triple
  , testCase "sort 4" $ sort' (mml [2, 1, 3]) @?= triple
  , testCase "sort 5" $ sort' (mml [10, 5, 1, 8, 3, 2, 6, 7, 4, 9]) @?= mml [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  , testCase "sort 6" $ sort' many @?= mml ["!", "Goodbye", "Hello", "World"]
  ]

sortByTests :: TestTree
sortByTests = testGroup "sortBy"
  [ testCase "sortBy 1" $ sortBy' compare Nil @?= (Nil :: MyList Int)
  , testCase "sortBy 2" $ sortBy' compare single @?= single
  , testCase "sortBy 3" $ sortBy' compare triple @?= triple
  , testCase "sortBy 4" $ sortBy' f (mml [2, 1, 3]) @?= mml [3, 1, 2]
  , testCase "sortBy 5" $ sortBy' f (mml [10, 5, 1, 8, 3, 2, 6, 7, 4, 9]) @?= mml [9, 7, 5, 3, 1, 10, 8, 6, 4, 2]
  , testCase "sortBy 6" $ sortBy' g many @?= mml ["Hello", "Goodbye", "World", "!"]
  ]
  where
    f x y = case (even x, even y) of
      (False, True) -> LT
      (True, False) -> GT
      _ -> y `compare` x
    g s1 s2 = last s2 `compare` last s1

sortOnTests :: TestTree
sortOnTests = testGroup "sortOn"
  [ testCase "sortOn 1" $ sortOn' (`mod` 3) (mml [4, 7, 1, 2, 5, 1, 3, 6, 2]) @?=
      mml [3, 6, 4, 7, 1, 1, 2, 5, 2]
  , testCase "sortOn 2" $ sortOn' (!! 2) (mml ["World", "There", "Hello", "Once", "Upon"]) @?=
      mml ["Once", "There", "Hello", "Upon", "World"]
  ]

groupTests :: TestTree
groupTests = testGroup "group"
  [ testCase "group 1" $ group' Nil @?= (Nil :: MyList (MyList Int))
  , testCase "group 2" $ group' single @?= mml [single]
  , testCase "group 3" $ group' double @?= mml [single, Cons 2 Nil]
  , testCase "group 4" $ group' (mml [3, 3, 4, 1, 1, 1, 5, 1, 4]) @?=
      mml [mml [3, 3], Cons 4 Nil, mml [1, 1, 1], Cons 5 Nil, Cons 1 Nil, Cons 4 Nil]
  , testCase "group 5" $ group' (mml "mississippi") @?=
      mml [mml ['m'], mml ['i'], mml ['s', 's'], mml ['i'], mml ['s', 's'], mml ['i'], mml ['p', 'p'], mml ['i']]
  ]

groupByTests :: TestTree
groupByTests = testGroup "groupBy"
  [ testCase "groupBy 1" $ groupBy' k (mml [4, 7, 1, 2, 5, 1, 3, 6, 2]) @?=
      mml [mml [4, 7, 1], mml [2, 5], mml [1], mml [3, 6], mml [2]]
  , testCase "groupBy 2" $ groupBy' h (mml "miSsisSipPi") @?=
      mml [mml ['m'], mml ['i'], mml ['S', 's'], mml ['i'], mml ['s', 'S'], mml ['i'], mml ['p', 'P'], mml ['i']]
  ]
  where
    k x y = x `mod` 3 == y `mod` 3
    h c1 c2 = toLower c1 == toLower c2

studentAwardsTests :: TestTree
studentAwardsTests = testGroup "studentAwards"
  [ testCase "studentAwards 1" $ studentAwards [] @?= ("", "", "")
  , testCase "studentAwards 2" $ studentAwards
      [Student "Jack" 10 10 10 10 10]
      @?= ("Jack", "Jack", "Jack")
  , testCase "studentAwards 3" $ studentAwards
      [ Student "Jack" 10 10 10 10 10
      , Student "Chris" 15 5 5 5 5
      ]
      @?= ("Chris", "Jack", "Jack")
  , testCase "studentAwards 4" $ studentAwards
      [ Student "Jack" 10 10 10 10 10
      , Student "Chris" 15 5 5 5 5
      , Student "Craig" 5 5 5 5 15
      ]
      @?= ("Chris", "Jack", "Craig")
  , testCase "studentAwards 5" $ studentAwards
      [ Student "Jack" 10 10 10 10 10
      , Student "Chris" 15 5 5 5 5
      , Student "Craig" 5 5 5 5 15
      , Student "Kelly" 5 5 5 25 10
      ]
      @?= ("Chris", "Kelly", "Craig")
  , testCase "studentAwards 6" $ studentAwards
      [ Student "Jack" 10 10 10 10 10
      , Student "Chris" 15 5 5 5 5
      , Student "Craig" 5 5 5 5 15
      , Student "Kelly" 5 5 5 25 10
      , Student "Ashley" 14 15 15 15 14
      ]
      @?= ("Chris", "Kelly", "Craig")
  ]

anyOverlapTests :: TestTree
anyOverlapTests = testGroup "anyOverlap"
  [ testCase "anyOverlap 1" $ anyOverlap [] @?= False
  , testCase "anyOverlap 2" $ anyOverlap [(1, 5)] @?= False
  , testCase "anyOverlap 3" $ anyOverlap [(1, 5), (5, 10)] @?= True
  , testCase "anyOverlap 4" $ anyOverlap [(5, 10), (1, 5)] @?= True
  , testCase "anyOverlap 5" $ anyOverlap [(1, 6), (4, 10)] @?= True
  , testCase "anyOverlap 6" $ anyOverlap [(4, 10), (1, 6)] @?= True
  , testCase "anyOverlap 7" $ anyOverlap [(4, 8), (9, 15)] @?= False
  , testCase "anyOverlap 8" $ anyOverlap [(4, 8), (9, 15), (20, 30), (31, 40), (41, 50)] @?= False
  , testCase "anyOverlap 9" $ anyOverlap [(4, 8), (9, 15), (20, 30), (31, 40), (1, 6)] @?= True
  ]

buildIntervalsTests :: TestTree
buildIntervalsTests = testGroup "buildIntervals"
  [ testCase "buildIntervals 1" $ buildIntervals 1 [] @?= []
  , testCase "buildIntervals 2" $ buildIntervals 0 [True, True, True] @?= [(0,2)]
  , testCase "buildIntervals 3" $ buildIntervals 1 [True, True, True] @?= [(1,3)]
  , testCase "buildIntervals 4" $ buildIntervals 1 [True, False, True] @?= [(1,1), (3,3)]
  , testCase "buildIntervals 5" $ buildIntervals 1 [False, True, True] @?= [(2,3)]
  , testCase "buildIntervals 6" $ buildIntervals 1 [True, True, False] @?= [(1,2)]
  , testCase "buildIntervals 7" $
      buildIntervals 2 [True, True, False, True, False, False, True, True, False] @?=
        [(2,3), (5,5), (8,9)]
  , testCase "buildIntervals 8" $
      buildIntervals 2 [True, True, False, True, False, True, True, True] @?=
        [(2,3), (5,5), (7,9)]
  ]

anagramsTests :: TestTree
anagramsTests = testGroup "anagrams"
  [ testCase "anagrams 1" $ anagrams [] @?= []
  , testCase "anagrams 2" $ anagrams ["ba", "ab"] @?= [["ab", "ba"]]
  , testCase "anagrams 3" $ anagrams ["b", "a", "d", "c"] @?= [["a"], ["b"], ["c"], ["d"]]
  , testCase "anagrams 4" $ anagrams ["tab", "table", "god", "dog", "bat", "abt"]
      @?= [["abt", "bat", "tab"], ["dog", "god"], ["table"]]
  , testCase "anagrams 5" $ anagrams ["tab", "table", "god", "dog", "tba"]
      @?= [["dog", "god"], ["tab", "tba"], ["table"]]
  , testCase "anagrams 6" $ anagrams ["cab", "box", "ox", "abc", "xo", "oxb"]
      @?= [["abc", "cab"], ["box", "oxb"], ["ox", "xo"]]
  ]

buildMazeTests :: TestTree
buildMazeTests = testGroup "buildMaze"
  [ testCase "buildMaze 1" $ buildMaze "." 1 @?= [[((0,0), False)]]
  , testCase "buildMaze 2" $ buildMaze "x" 1 @?= [[((0,0), True)]]
  , testCase "buildMaze 3" $ buildMaze ".x" 1 @?= [[((0,0), False), ((0,1), True)]]
  , testCase "buildMaze 4" $ buildMaze ".x" 2 @?= [[((0,0), False)], [((1,0), True)]]
  , testCase "buildMaze 5" $ buildMaze "..x." 2 @?=
      [[((0,0), False), ((0,1), False)], [((1,0), True), ((1,1), False)]]
  , testCase "buildMaze 6" $ buildMaze "..x...x.." 3 @?=
      [ [((0,0), False), ((0,1), False), ((0,2), True)]
      , [((1,0), False), ((1,1), False), ((1,2), False)]
      , [((2,0), True), ((2,1), False), ((2,2), False)]
      ]
  , testCase "buildMaze 7" $ buildMaze "..x...x.x..." 3 @?=
      [ [((0,0), False), ((0,1), False), ((0,2), True), ((0,3), False)]
      , [((1,0), False), ((1,1), False), ((1,2), True), ((1,3), False)]
      , [((2,0), True), ((2,1), False), ((2,2), False), ((2,3), False)]
      ]
  , testCase "buildMaze 8" $ buildMaze "..x...x.x..." 4 @?=
      [ [((0,0), False), ((0,1), False), ((0,2), True)]
      , [((1,0), False), ((1,1), False), ((1,2), False)]
      , [((2,0), True), ((2,1), False), ((2,2), True)]
      , [((3,0), False), ((3,1), False), ((3,2), False)]
      ]
  ]

incrementingChunksTests :: TestTree
incrementingChunksTests = testGroup "incrementingChunks"
  [ testCase "incrementingChunks 1" $ incrementingChunks [] @?= []
  , testCase "incrementingChunks 2" $ incrementingChunks [1,2,3] @?= [[1,2,3]]
  , testCase "incrementingChunks 3" $ incrementingChunks [1,3] @?= [[1],[3]]
  , testCase "incrementingChunks 4" $ incrementingChunks [1,1,1] @?= [[1],[1],[1]]
  , testCase "incrementingChunks 5" $ incrementingChunks [3,2,1] @?= [[3],[2],[1]]
  , testCase "incrementingChunks 6" $ incrementingChunks [1,2,5,2,4,5,6,7,10,11] @?=
      [[1,2], [5], [2], [4,5,6,7], [10,11]]
  , testCase "incrementingChunks 7" $ incrementingChunks [1,2,5,2,4,5,6,7,7,10,10,11] @?=
      [[1,2], [5], [2], [4,5,6,7], [7], [10], [10,11]]
  ]
