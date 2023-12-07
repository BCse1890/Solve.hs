module Main where

import Test.Tasty
import Test.Tasty.HUnit

import M1Lecture3
import MyList
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 3 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ lengthTests
  , sumTests
  , productTests
  , maximumTests
  , minimumTests
  , elemIndexTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ makeDigitsTests
  , buildNumberTests
  , elevationTests
  , viralCountTests
  , addQuadsTests
  , addQuadsTests
  , countCharsTests
  ]

lengthTests :: TestTree
lengthTests = testGroup "length"
  [ testCase "length 1" $ length' Nil @?= 0
  , testCase "length 2" $ length' single @?= 1
  , testCase "length 3" $ length' double @?= 2
  , testCase "length 4" $ length' triple @?= 3
  , testCase "length 5" $ length' many @?= 4
  ]

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "sum 1" $ sum' Nil @?= 0
  , testCase "sum 2" $ sum' single @?= 1
  , testCase "sum 3" $ sum' double @?= 3
  , testCase "sum 4" $ sum' triple @?= 6
  , testCase "sum 5" $ sum' (mml [2, 6, 9, 15]) @?= 32
  ]

productTests :: TestTree
productTests = testGroup "product"
  [ testCase "product 1" $ product' Nil @?= 1
  , testCase "product 2" $ product' single @?= 1
  , testCase "product 3" $ product' double @?= 2
  , testCase "product 4" $ product' triple @?= 6
  , testCase "product 5" $ product' (mml [2, 6, 9, 15]) @?= 1620
  ]

maximumTests :: TestTree
maximumTests = testGroup "maximum"
  [ testCase "maximum 1" $ maximum' single @?= 1
  , testCase "maximum 2" $ maximum' double @?= 2
  , testCase "maximum 3" $ maximum' triple @?= 3
  , testCase "maximum 4" $ maximum' (mml [3, 5, 2, 1, 0]) @?= 5
  , testCase "maximum 5" $ maximum' many @?= "World"
  ]

minimumTests :: TestTree
minimumTests = testGroup "minimum"
  [ testCase "minimum 1" $ minimum' single @?= 1
  , testCase "minimum 2" $ minimum' double @?= 1
  , testCase "minimum 3" $ minimum' triple @?= 1
  , testCase "minimum 4" $ minimum' (mml [3, 5, 2, 1, 0]) @?= 0
  , testCase "minimum 5" $ minimum' many @?= "!"
  ]

elemIndexTests :: TestTree
elemIndexTests = testGroup "elemIndex"
  [ testCase "elemIndex 1" $ elemIndex' 1 single @?= Just 0
  , testCase "elemIndex 2" $ elemIndex' 2 single @?= Nothing
  , testCase "elemIndex 3" $ elemIndex' 2 triple @?= Just 1
  , testCase "elemIndex 3" $ elemIndex' 3 triple @?= Just 2
  , testCase "elemIndex 4" $ elemIndex' 1 (mml [3, 5, 2, 1, 0]) @?= Just 3
  , testCase "elemIndex 4" $ elemIndex' 7 (mml [3, 5, 2, 1, 0]) @?= Nothing
  , testCase "elemIndex 5" $ elemIndex' "!" many @?= Just 2
  , testCase "elemIndex 5" $ elemIndex' "Word" many @?= Nothing
  ]

makeDigitsTests :: TestTree
makeDigitsTests = testGroup "makeDigits"
  [ testCase "makeDigits 1" $ makeDigits 0 @?= []
  , testCase "makeDigits 2" $ makeDigits 5 @?= [5]
  , testCase "makeDigits 3" $ makeDigits 16 @?= [6, 1]
  , testCase "makeDigits 4" $ makeDigits 3042 @?= [2, 4, 0, 3]
  ]

buildNumberTests :: TestTree
buildNumberTests = testGroup "buildNumber"
  [ testCase "buildNumber 1" $ buildNumber [] @?= 0
  , testCase "buildNumber 2" $ buildNumber [0] @?= 0
  , testCase "buildNumber 3" $ buildNumber [9] @?= 9
  , testCase "buildNumber 4" $ buildNumber [2, 3] @?= 23
  , testCase "buildNumber 5" $ buildNumber [2, 4, 0, 3] @?= 2403
  , testCase "buildNumber 6" $ buildNumber [8, 5, 7, 0, 3, 2, 1, 9] @?= 85703219
  ]

elevationTests :: TestTree
elevationTests = testGroup "elevation"
  [ testCase "elevation 1" $ elevation "" @?= 0
  , testCase "elevation 2" $ elevation "duddudd" @?= (-300)
  , testCase "elevation 4" $ elevation
      "uuuuuuuuuuddddddddddddddduuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuudddddddddduuduuduuduuduud"
      @?= 2500
  ]

viralCountTests :: TestTree
viralCountTests = testGroup "viralCount"
  [ testCase "viralCount 1" $ viralCount 15 0 @?= 0
  , testCase "viralCount 2" $ viralCount 15 1 @?= 5
  , testCase "viralCount 3" $ viralCount 15 2 @?= 11
  , testCase "viralCount 4" $ viralCount 15 3 @?= 19
  , testCase "viralCount 5" $ viralCount 13 3 @?= 15
  , testCase "viralCount 6" $ viralCount 20 24 @?= 14724
  , testCase "viralCount 7" $ viralCount 7 24 @?= 48
  ]

addQuadsTests :: TestTree
addQuadsTests = testGroup "addQuads"
  [ testCase "addQuads 1" $ addQuads [] [] @?= 0
  , testCase "addQuads 2" $ addQuads [] [7] @?= 7
  , testCase "addQuads 3" $ addQuads [] [7, 4] @?= 28
  , testCase "addQuads 4" $ addQuads [2] [7, 4] @?= 56
  , testCase "addQuads 5" $ addQuads [2, 2] [7, 4] @?= 112
  , testCase "addQuads 6" $ addQuads [7] [] @?= 7
  , testCase "addQuads 7" $ addQuads [7, 9] [] @?= 63
  , testCase "addQuads 8" $ addQuads [7, 4] [3] @?= 84
  , testCase "addQuads 9" $ addQuads [7, 9, 2] [] @?= 65
  , testCase "addQuads 10" $ addQuads [7, 9, 2] [2, 1, 5] @?= 136
  , testCase "addQuads 11" $ addQuads [1, 6, 5, 3, 2] [8, 3, 2, -3, 4, 7, 9] @?= 119
  , testCase "addQuads 12" $ addQuads [8, 3, 2, -3, 4, 7, 9] [1, 6, 5, 3, 2] @?= 119
  ]

countCharsTests :: TestTree
countCharsTests = testGroup "countChars"
  [ testCase "countChars 1" $ countChars "abcd" @?= 4
  , testCase "countChars 2" $ countChars "\\o123" @?= 1
  , testCase "countChars 3" $ countChars "\\x12" @?= 1
  , testCase "countChars 4" $ countChars "ace\\\\bcd" @?= 7
  , testCase "countChars 5" $ countChars "ace\\\"bcd and" @?= 11
  , testCase "countChars 6" $ countChars "\\o761abc" @?= 4
  , testCase "countChars 7" $ countChars "\\xf9abc" @?= 4
  , testCase "countChars 8" $ countChars "\\xf9abc\\o013b\\\"" @?= 7
  ]
