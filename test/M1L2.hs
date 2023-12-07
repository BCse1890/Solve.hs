{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import M1Lecture2
import MyList
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 2 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ atIndexTests
  , lastTests
  , elemTests
  , findTests
  , andTests
  , orTests
  , allTests
  , anyTests
  , isPrefixOfTests
  , isInfixOfTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests" 
  [ parallelCarsTests
  , findKeyTests
  , firstProperFactorTests
  , areAllFactorsTests
  , hasIncrementingListTests
  , hashUntilIncrementTests
  ]

atIndexTests :: TestTree
atIndexTests = testGroup "atIndex"
  [ testCase "atIndex 1" $ atIndex single 0 @?= 1
  , testCase "atIndex 2" $ atIndex double 0 @?= 1
  , testCase "atIndex 3" $ atIndex double 1 @?= 2
  , testCase "atIndex 4" $ atIndex many 3 @?= "Goodbye"
  ]

lastTests :: TestTree
lastTests = testGroup "last"
  [ testCase "last 1" $ last' single @?= 1
  , testCase "last 2" $ last' double @?= 2
  , testCase "last 3" $ last' triple @?= 3
  , testCase "last 4" $ last' many @?= "Goodbye"
  ]

elemTests :: TestTree
elemTests = testGroup "elem"
  [ testCase "elem 1" $ elem' 1 Nil @?= False
  , testCase "elem 2" $ elem' 1 single @?= True
  , testCase "elem 3" $ elem' 2 single @?= False
  , testCase "elem 4" $ elem' 1 double @?= True
  , testCase "elem 5" $ elem' 2 double @?= True
  , testCase "elem 6" $ elem' "Hello" many @?= True
  , testCase "elem 7" $ elem' "World!" many @?= False
  ]

findTests :: TestTree
findTests = testGroup "find"
  [ testCase "find 1" $ find' (== 1) Nil @?= Nothing
  , testCase "find 2" $ find' (== 1) double @?= Just 1
  , testCase "find 3" $ find' even double @?= Just 2
  , testCase "find 4" $ find' (\s -> length s > 5) many @?= Just "Goodbye"
  , testCase "find 5" $ find' (\s -> not (null s) && head s == 'h') many @?= Nothing
  ]

andTests :: TestTree
andTests = testGroup "and"
  [ testCase "and 1" $ and' Nil @?= True
  , testCase "and 2" $ and' (Cons True Nil) @?= True
  , testCase "and 3" $ and' (Cons True (Cons False Nil)) @?= False
  ]

orTests :: TestTree
orTests = testGroup "or"
  [ testCase "or 1" $ or' Nil @?= False
  , testCase "or 2" $ or' (Cons True Nil) @?= True
  , testCase "or 3" $ or' (Cons True (Cons False Nil)) @?= True
  , testCase "or 3" $ or' (Cons False (Cons False Nil)) @?= False
  ]

allTests :: TestTree
allTests = testGroup "all"
  [ testCase "all 1" $ all' even Nil @?= True
  , testCase "all 2" $ all' even triple @?= False
  , testCase "all 3" $ all' (\s -> length s < 10) many @?= True
  , testCase "all 4" $ all' (\s -> length s > 2) many @?= False
  ]

anyTests :: TestTree
anyTests = testGroup "any"
  [ testCase "any 1" $ any' even Nil @?= False
  , testCase "any 2" $ any' even triple @?= True
  , testCase "any 3" $ any' (\s -> length s < 10) many @?= True
  , testCase "any 4" $ any' (\s -> length s > 2) many @?= True
  , testCase "any 5" $ any' null many @?= False
  ]

isPrefixOfTests :: TestTree
isPrefixOfTests = testGroup "isPrefixOf"
  [ testCase "isPrefixOf 1" $ isPrefixOf' (Nil :: MyList Int) Nil @?= True
  , testCase "isPrefixOf 2" $ isPrefixOf' (Cons 1 Nil) Nil @?= False
  , testCase "isPrefixOf 3" $ isPrefixOf' Nil double @?= True
  , testCase "isPrefixOf 4" $ isPrefixOf' single double @?= True
  , testCase "isPrefixOf 5" $ isPrefixOf' double single @?= False
  , testCase "isPrefixOf 6" $ isPrefixOf' double double @?= True
  , testCase "isPrefixOf 7" $ isPrefixOf' double triple @?= True
  , testCase "isPrefixOf 8" $ isPrefixOf' (Cons 2 Nil) triple @?= False
  , testCase "isPrefixOf 9" $ isPrefixOf' (Cons 2 (Cons 3 Nil)) triple @?= False
  , testCase "isPrefixOf 10" $ isPrefixOf' (mml ["Hello", "World"]) many @?= True
  , testCase "isPrefixOf 11" $ isPrefixOf' (mml ["World", "!"]) many @?= False
  ]

isInfixOfTests :: TestTree
isInfixOfTests = testGroup "isInfixOf"
  [ testCase "isInfixOf 1" $ isInfixOf' (Nil :: MyList Int) Nil @?= True
  , testCase "isInfixOf 2" $ isInfixOf' (Cons 1 Nil) Nil @?= False
  , testCase "isInfixOf 3" $ isInfixOf' (Cons 1 Nil) (mml [2, 1, 3]) @?= True
  , testCase "isInfixOf 4" $ isInfixOf' Nil double @?= True
  , testCase "isInfixOf 5" $ isInfixOf' single double @?= True
  , testCase "isInfixOf 6" $ isInfixOf' double single @?= False
  , testCase "isInfixOf 7" $ isInfixOf' double double @?= True
  , testCase "isInfixOf 8" $ isInfixOf' double triple @?= True
  , testCase "isInfixOf 9" $ isInfixOf' (Cons 2 Nil) triple @?= True
  , testCase "isInfixOf 10" $ isInfixOf' (Cons 2 (Cons 3 Nil)) (mml [1, 2, 3, 4]) @?= True
  , testCase "isInfixOf 11" $ isInfixOf' (mml ["World", "!"]) many @?= True
  , testCase "isInfixOf 12" $ isInfixOf' (mml ["Hello", "!"]) many @?= False
  ]

parallelCarsTests :: TestTree
parallelCarsTests = testGroup "parallelCars"
  [ testCase "parallelCars 1" $ parallelCars (1, 3) (5, 1) @?= True
  , testCase "parallelCars 2" $ parallelCars (5, 1) (1, 3) @?= True
  , testCase "parallelCars 3" $ parallelCars (5, 4) (7, 5) @?= False
  , testCase "parallelCars 4" $ parallelCars (7, 5) (5, 4) @?= False
  , testCase "parallelCars 5" $ parallelCars (2, 4) (9, 1) @?= False
  , testCase "parallelCars 6" $ parallelCars (9, 1) (2, 4) @?= False
  , testCase "parallelCars 7" $ parallelCars (90, 1) (0, 10) @?= True
  , testCase "parallelCars 8" $ parallelCars (10, 20) (280, 2) @?= True
  ]

findKeyTests :: TestTree
findKeyTests = testGroup "findKey"
  [ testCase "findKey 1" $ findKey "" "Hello" @?= True
  , testCase "findKey 2" $ findKey "" "Goodbye" @?= True
  , testCase "findKey 3" $ findKey "H" "" @?= False
  , testCase "findKey 4" $ findKey "Hello" "Help" @?= False
  , testCase "findKey 5" $ findKey "aa" "a" @?= True
  , testCase "findKey 6" $ findKey "world" "abcdefghijklmnopqrstuvwxyz" @?= True
  , testCase "findKey 7" $ findKey "WORLD" "abcdefghijklmnopqrstuvwxyz" @?= False
  , testCase "findKey 8" $ findKey "world" "abcefghijklmnopqrstuvwxyz" @?= False
  , testCase "findKey 9" $ findKey "pghbjqieeee" "pbidkanwgwkricndnjqhehdnaj" @?= True
  , testCase "findKey 10" $ findKey "pghbjqiexx" "pbidkanwgwkricndnjqhehdnaj" @?= False
  ]

firstProperFactorTests :: TestTree
firstProperFactorTests = testGroup "firstProperFactor"
  [ testCase "firstProperFactor 1" $ firstProperFactor [] @?= Nothing
  , testCase "firstProperFactor 2" $ firstProperFactor [6, 1, 8] @?= Just 1
  , testCase "firstProperFactor 3" $ firstProperFactor [8] @?= Nothing
  , testCase "firstProperFactor 4" $ firstProperFactor [3, 5, 8, 8] @?= Nothing
  , testCase "firstProperFactor 5" $ firstProperFactor [3, 5, 4, 8] @?= Just 4
  , testCase "firstProperFactor 6" $ firstProperFactor [2, 5, 4, 8] @?= Just 2
  , testCase "firstProperFactor 7" $ firstProperFactor [5, 7, 24, 8, 9, 12, 24] @?= Just 8
  , testCase "firstProperFactor 8" $ firstProperFactor [9, 13, 24, 24] @?= Nothing
  ]

areAllFactorsTests :: TestTree
areAllFactorsTests = testGroup "areAllFactors"
  [ testCase "areAllFactors 1" $ areAllFactors [] [1, 2, 3] @?= True
  , testCase "areAllFactors 2" $ areAllFactors [0, 2] [2, 1, 3] @?= True
  , testCase "areAllFactors 3" $ areAllFactors [1] [10, 12, 14] @?= True
  , testCase "areAllFactors 4" $ areAllFactors [0] [10, 12, 14] @?= False
  , testCase "areAllFactors 5" $ areAllFactors [0, 1] [10, 12, 14] @?= False
  , testCase "areAllFactors 6" $ areAllFactors [1, 3] [10, 12, 14, 16, 18, 26] @?= True
  , testCase "areAllFactors 7" $ areAllFactors [1, 3, 5] [10, 12, 14, 16, 18, 26] @?= False
  , testCase "areAllFactors 8" $ areAllFactors [0] [10, 12, 14, 16, 18, 26] @?= False
  ]

hasIncrementingListTests :: TestTree
hasIncrementingListTests = testGroup "hasIncrementingList"
  [ testCase "hasIncrementingList 1" $ hasIncrementingList [] [1, 2, 3] @?= False
  , testCase "hasIncrementingList 2" $ hasIncrementingList [1] [1, 2, 3] @?= True
  , testCase "hasIncrementingList 3" $ hasIncrementingList [2] [1, 2, 3] @?= False
  , testCase "hasIncrementingList 4" $ hasIncrementingList [1] [] @?= False
  , testCase "hasIncrementingList 5" $ hasIncrementingList [1] ls @?= True
  , testCase "hasIncrementingList 6" $ hasIncrementingList [1, 18] ls @?= True
  , testCase "hasIncrementingList 7" $ hasIncrementingList [19] ls @?= True
  , testCase "hasIncrementingList 8" $ hasIncrementingList [42, 20, 5, 17, 43] ls @?= False
  ]
  where
    ls = [5, 1, 2, 3, 17, 43, 18, 19, 20, 21, 42, 43]

hashUntilIncrementTests :: TestTree
hashUntilIncrementTests = testGroup "hashUntilIncrement"
  [ testCase "hashUntilIncrement 1" $ hashUntilIncrement "abcdef" 0 @?= 1416764
  , testCase "hashUntilIncrement 2" $ hashUntilIncrement "igneqy" 0 @?= 2140555
  , testCase "hashUntilIncrement 3" $ hashUntilIncrement "ponfeq" 0 @?= 435450
  , testCase "hashUntilIncrement 4" $ hashUntilIncrement "ponfeq" 435451 @?= 695660
  ]
