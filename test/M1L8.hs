module Main where

import Data.Char (toLower, toUpper)
import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture8
import Utils
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 8 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ nubTests
  , deleteTests
  , intersectTests
  , unionTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ commonTokenTests
  , compartmentalizeTests
  , traverse2DTests
  , smallestMultiplesTests
  , grabbingLettersTests
  , grabbingMoreLettersTests
  ]

nubTests :: TestTree
nubTests = testGroup "nub"
  [ testCase "nub 1" $ nub' Nil @?= (Nil :: MyList Int)
  , testCase "nub 2" $ nub' triple @?= triple
  , testCase "nub 3" $ nub' (mml [1, 2, 1, 3, 2, 1, 4]) @?= mml [1, 2, 3, 4]
  , testCase "nub 4" $ nub' many @?= many
  , testCase "nub 5" $ nub' (mml ["B", "A", "A", "B"]) @?= mml ["B", "A"]
  ]

deleteTests :: TestTree
deleteTests = testGroup "delete"
  [ testCase "delete 1" $ delete' 1 Nil @?= (Nil :: MyList Int)
  , testCase "delete 2" $ delete' 3 triple @?= double
  , testCase "delete 3" $ delete' 2 double @?= single
  , testCase "delete 4" $ delete' 1 (mml [1, 2, 1, 3, 2, 1, 4]) @?= mml [2, 1, 3, 2, 1, 4]
  , testCase "delete 5" $ delete' 2 (mml [1, 2, 1, 3, 2, 1, 4]) @?= mml [1, 1, 3, 2, 1, 4]
  , testCase "delete 6" $ delete' "!" many @?= mml ["Hello", "World", "Goodbye"]
  , testCase "delete 7" $ delete' "B" (mml ["B", "A", "A", "B"]) @?= mml ["A", "A", "B"]
  ]

intersectTests :: TestTree
intersectTests = testGroup "intersect"
  [ testCase "intersect 1" $ intersect' Nil Nil @?= (Nil :: MyList Int)
  , testCase "intersect 2" $ intersect' single Nil @?= Nil
  , testCase "intersect 3" $ intersect' Nil single @?= Nil
  , testCase "intersect 4" $ intersect' double single @?= single
  , testCase "intersect 5" $ intersect' single double @?= single
  , testCase "intersect 6" $ intersect' double triple @?= double
  , testCase "intersect 7" $ intersect' (mml [1, 2, 3, 4, 5]) (mml [2, 2, 9, 5, 3]) @?= mml [2, 3, 5]
  , testCase "intersect 8" $ intersect' (mml [2, 2, 9, 5, 3]) (mml [1, 2, 3, 4, 5]) @?= mml [2, 2, 5, 3]
  , testCase "intersect 9" $ intersect' many many @?= many
  ]

unionTests :: TestTree
unionTests = testGroup "union"
  [ testCase "union 1" $ union' Nil Nil @?= (Nil :: MyList Int)
  , testCase "union 2" $ union' single Nil @?= single
  , testCase "union 3" $ union' single single @?= single
  , testCase "union 4" $ union' double single @?= double
  , testCase "union 5" $ union' single double @?= double
  , testCase "union 6" $ union' double triple @?= triple
  , testCase "union 7" $ union' (mml [1, 1, 2]) (mml [1, 3, 2, 3]) @?= mml [1, 1, 2, 3]
  , testCase "union 8" $ union' (mml [2, 2, 9, 5, 3]) (mml [1, 2, 3, 4, 5]) @?=
      mml [2, 2, 9, 5, 3, 1, 4]
  ]

commonTokenTests :: TestTree
commonTokenTests = testGroup "commonToken"
  [ testCase "commonToken 1" $ commonToken [] @?= ""
  , testCase "commonToken 2" $ commonToken ["ab", "ac", "ad"] @?= "a"
  , testCase "commonToken 3" $ commonToken ["ab", "bc", "ba"] @?= "b"
  , testCase "commonToken 4" $ commonToken ["ab", "ac", "ad", "abc", "cbd", "dca"] @?= "ac"
  , testCase "commonToken 5" $ commonToken
      ["hallway", "wink", "wolf", "iced", "inked", "it", "new", "network", "nut"] @?= "win"
  , testCase "commonToken 6" $ commonToken
      [ "input", "reject", "tornado", "realize", "criminal", "isotope"
      , "left", "egregious", "rife"] @?= "tie"
  , testCase "commonToken 7" $ commonToken
      ["a", "ab", "ac", "b", "b", "b", "c", "c", "c", "d", "d", "d"] @?= "abcd"
  ]

compartmentalizeTests :: TestTree
compartmentalizeTests = testGroup "compartmentalize"
  [ testCase "compartmentalize 1" $ compartmentalize "abad" 2 @?= Just 'a'
  , testCase "compartmentalize 2" $ compartmentalize "abadcdup" 2 @?= Just 'd'
  , testCase "compartmentalize 3" $ compartmentalize "abadcdua" 2 @?= Nothing
  , testCase "compartmentalize 4" $ compartmentalize "abcdefgh" 2 @?= Nothing
  , testCase "compartmentalize 5" $ compartmentalize "hilhugmuh" 3 @?= Just 'h'
  , testCase "compartmentalize 6" $ compartmentalize "ibegpgdeniemquem" 4 @?= Just 'e'
  ]

traverse2DTests :: TestTree
traverse2DTests = testGroup "traverse2D"
  [ testCase "traverse2D 1" $ traverse2D (0,0) [] @?= 1
  , testCase "traverse2D 2" $ traverse2D (2,3) [Up] @?= 2
  , testCase "traverse2D 3" $ traverse2D (0,0) [Up, Down, Left, Right] @?= 3
  , testCase "traverse2D 4" $ traverse2D (1,1) [Up, Right, Down, Left] @?= 4
  , testCase "traverse2D 5" $ traverse2D (0,0) [Up, Right, Down, Down] @?= 5
  , testCase "traverse2D 6" $ traverse2D (1,1)
      [Up,Right,Down,Left,Up,Left,Down,Right,Up,Up] @?= 7
  ]

smallestMultiplesTests :: TestTree
smallestMultiplesTests = testGroup "smallestMultiples"
  [ testCase "smallestMultiples 1" $ smallestMultiples [] [] @?= []
  , testCase "smallestMultiples 1" $ smallestMultiples [1,2,3] [] @?= []
  , testCase "smallestMultiples 1" $ smallestMultiples [] [1,2,3] @?= []
  , testCase "smallestMultiples 1" $ smallestMultiples [1,2,3] [1,2,3] @?= [1,2,3]
  , testCase "smallestMultiples 1" $ smallestMultiples [64, 28, 29, 33] [1,2,3] @?= [28, 64, 33]
  , testCase "smallestMultiples 1" $ smallestMultiples [64, 28, 29, 33] [2,3,4,11,29] @?=
      [28, 33, 64, 29]
  , testCase "smallestMultiples 1" $ smallestMultiples [64] [4,4,16,11,29] @?=
      [64]
  , testCase "smallestMultiples 1" $ smallestMultiples [64, 2, 32, 58, 55] [4,4,16,11,29] @?=
      [32, 64, 55, 58]
  , testCase "smallestMultiples 1" $ smallestMultiples [64,64] [4,11,16,29] @?=
      [64,64]
  , testCase "smallestMutliples 1" $ smallestMultiples [16, 9, 12, 34, 26, 18] [4, 17, 3, 5, 6]
      @?= [12, 34, 9, 18]
  ]

grabbingLettersTests :: TestTree
grabbingLettersTests = testGroup "grabbingLetters"
  [ testCase "grabbingLetters 1" $ grabbingLetters ["ba", "cb", "dc"] (1, 1, 1) @?= "abcd"
  , testCase "grabbingLetters 2" $ grabbingLetters
      ["ab", "ca", "bc", "ehis", "zafg"] (1, 2, 3) @?= "abcfgz"
  , testCase "grabbingLetters 3" $ grabbingLetters
      ["ab", "ca", "bc", "ehis", "zafg"] (1, 1, 1) @?= "abc"
  , testCase "grabbingLetters 4" $ grabbingLetters
      ["ab", "ca", "bc", "ehis", "zafg"] (1, 3, 1) @?= "abcehis"
  , testCase "grabbingLetters 5" $ grabbingLetters
      ["z", "y", "x", "a", "b", "c", "d", "m", "o"] (9,8,7) @?= "dmo"
  , testCase "grabbingLetters 6" $ grabbingLetters
      ["z", "y", "x", "a", "b", "c", "d", "m", "o"] (7,8,7) @?= "dmo"
  , testCase "grabbingLetters 7" $ grabbingLetters
      ["z", "y", "x", "a", "b", "c", "d", "m", "o"] (5,5,5) @?= "bcd"
  ]

grabbingMoreLettersTests :: TestTree
grabbingMoreLettersTests = testGroup "grabbingMoreLetters"
  [ testCase "grabbingMoreLetters 1" $ grabbingMoreLetters ["ba", "cb", "dc"] @?= "abcd"
  , testCase "grabbingMoreLetters 2" $ grabbingMoreLetters
      ["a", "z", "f", "c", "b", "y", "x"] @?= "abc"
  , testCase "grabbingMoreLetters 3" $ grabbingMoreLetters
      ["ab", "ca", "bc", "ehis", "zafg"] @?= "abcefghisz"
  , testCase "grabbingMoreLetters 4" $ grabbingMoreLetters
      ["ab", "caaaa", "bbbbbc", "ehis", "zafg"] @?= "abcefghisz"
  , testCase "grabbingMoreLetters 5" $ grabbingMoreLetters
      ["ab", "cancancancan", "bcpm", "ehis", "zafg"] @?= "abcefghimpsz"
  ]
