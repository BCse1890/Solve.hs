module Main where

import Data.Char (toLower, toUpper)
import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture5
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 5 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ reverseTests
  , appendTests
  , findIndicesTests
  , isSuffixOfTests
  , mapTests
  , filterTests
  , snocTests
  , initTests
  , concatTests
  , concatMapTests
  , zipTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ enumerateTests
  , calculateVelocitiesTests
  , palindromeTests
  , makeDigitsTests
  , allFactorsTests
  , canStartShedTests
  ]

reverseTests :: TestTree
reverseTests = testGroup "reverse"
  [ testCase "reverse 1" $ reverse' Nil @?= (Nil :: MyList Int)
  , testCase "reverse 2" $ reverse' single @?= single
  , testCase "reverse 3" $ reverse' double @?= mml [2, 1]
  , testCase "reverse 4" $ reverse' triple @?= mml [3, 2, 1]
  , testCase "reverse 5" $ reverse' many @?= mml ["Goodbye", "!", "World", "Hello"]
  ]

appendTests :: TestTree
appendTests = testGroup "append"
  [ testCase "append 1" $ append' Nil Nil @?= (Nil :: MyList Int)
  , testCase "append 2" $ append' single Nil @?= single
  , testCase "append 3" $ append' Nil single @?= single
  , testCase "append 4" $ append' single double @?= mml [1, 1, 2]
  , testCase "append 5" $ append' triple double @?= mml [1, 2, 3, 1, 2]
  , testCase "append 6" $ append' many (Cons "World!" Nil) @?= mml ["Hello", "World", "!", "Goodbye", "World!"]
  ]

findIndicesTests :: TestTree
findIndicesTests = testGroup "findIndices"
  [ testCase "findIndices 1" $ findIndices' even Nil @?= Nil
  , testCase "findIndices 2" $ findIndices' even single @?= Nil
  , testCase "findIndices 3" $ findIndices' even double @?= Cons 1 Nil
  , testCase "findIndices 4" $ findIndices' odd triple @?= mml [0, 2]
  , testCase "findIndices 5" $ findIndices' (\s -> length s > 2) many @?= mml [0, 1, 3]
  , testCase "findIndices 6" $ findIndices' (== 5) (mml [1, 5, 2, 5, 5]) @?= mml [1, 3, 4]
  ]

isSuffixOfTests :: TestTree
isSuffixOfTests = testGroup "isSuffixOf"
  [ testCase "isSuffixOf 1" $ isSuffixOf' (Nil :: MyList Int) Nil @?= True
  , testCase "isSuffixOf 2" $ isSuffixOf' Nil double @?= True
  , testCase "isSuffixOf 3" $ isSuffixOf' (Cons 2 Nil) double @?= True
  , testCase "isSuffixOf 4" $ isSuffixOf' (Cons 3 Nil) triple @?= True
  , testCase "isSuffixOf 5" $ isSuffixOf' (mml [2, 3]) triple @?= True
  , testCase "isSuffixOf 6" $ isSuffixOf' triple triple @?= True
  , testCase "isSuffixOf 7" $ isSuffixOf' double triple @?= False
  , testCase "isSuffixOf 8" $ isSuffixOf' (mml [1, 3]) triple @?= False
  , testCase "isSuffixOf 9" $ isSuffixOf' (mml ["!", "Goodbye"]) many @?= True
  , testCase "isSuffixOf 10" $ isSuffixOf' (mml ["Hello"]) many @?= False
  ]

mapTests :: TestTree
mapTests = testGroup "map"
  [ testCase "map 1" $ map' (*2) Nil @?= Nil
  , testCase "map 2" $ map' (*2) single @?= Cons 2 Nil
  , testCase "map 3" $ map' (*2) double @?= mml [2, 4]
  , testCase "map 4" $ map' (*2) triple @?= mml [2, 4, 6]
  , testCase "map 5" $ map' show triple @?= mml ["1", "2", "3"]
  , testCase "map 6" $ map' head many @?= mml ['H', 'W', '!', 'G']
  ]

filterTests :: TestTree
filterTests = testGroup "filter"
  [ testCase "filter 1" $ filter' even Nil @?= Nil
  , testCase "filter 2" $ filter' even single @?= Nil
  , testCase "filter 3" $ filter' even double @?= Cons 2 Nil
  , testCase "filter 4" $ filter' odd triple @?= mml [1, 3]
  , testCase "filter 5" $ filter' (\s -> length s > 2) many @?= mml ["Hello", "World", "Goodbye"]
  , testCase "filter 6" $ filter' (== 5) (mml [1, 5, 2, 5, 5]) @?= mml [5, 5, 5]
  ]

snocTests :: TestTree
snocTests = testGroup "snoc"
  [ testCase "snoc 1" $ snoc' Nil 1 @?= Cons 1 Nil
  , testCase "snoc 2" $ snoc' single 5 @?= mml [1, 5]
  , testCase "snoc 3" $ snoc' triple 5 @?= mml [1, 2, 3, 5]
  , testCase "snoc 4" $ snoc' many "World!" @?= mml ["Hello", "World", "!", "Goodbye", "World!"]
  ]

initTests :: TestTree
initTests = testGroup "init"
  [ testCase "init 1" $ init' single @?= Nil
  , testCase "init 2" $ init' double @?= Cons 1 Nil
  , testCase "init 3" $ init' triple @?= mml [1, 2]
  , testCase "init 4" $ init' many @?= mml ["Hello", "World", "!"]
  ]

concatTests :: TestTree
concatTests = testGroup "concat"
  [ testCase "concat 1" $ concat' (mml [Nil, Nil]) @?= (Nil :: MyList Int)
  , testCase "concat 2" $ concat' (mml [Nil, double]) @?= double
  , testCase "concat 3" $ concat' (mml [double, Nil]) @?= double
  , testCase "concat 4" $ concat' (mml [double, triple]) @?= mml [1, 2, 1, 2, 3]
  , testCase "concat 5" $ concat' (mml [triple, double, single]) @?= mml [1, 2, 3, 1, 2, 1]
  , testCase "concat 6" $ concat' (mml [many, many]) @?=
      mml ["Hello", "World", "!", "Goodbye", "Hello", "World", "!", "Goodbye"]
  ]

concatMapTests :: TestTree
concatMapTests = testGroup "concatMap"
  [ testCase "concatMap 1" $ concatMap' f Nil @?= Nil
  , testCase "concatMap 2" $ concatMap' f single @?= mml [2, 3, 4]
  , testCase "concatMap 3" $ concatMap' f double @?= mml [2, 3, 4, 3, 4, 5]
  , testCase "concatMap 4" $ concatMap' f triple @?= mml [2, 3, 4, 3, 4, 5, 4, 5, 6]
  , testCase "concatMap 5" $ concatMap' g many @?=
      mml ["hello", "HELLO", "world", "WORLD", "!", "!", "goodbye", "GOODBYE"]
  ]
  where
    f x = mml [x + 1, x + 2, x + 3]
    g s = mml [map toLower s, map toUpper s] 

zipTests :: TestTree
zipTests = testGroup "zip"
  [ testCase "zip 1" $ zip' (mml [] :: MyList Int) (mml [] :: MyList String) @?= mml []
  , testCase "zip 2" $ zip' (mml ["Hi"]) (mml [] :: MyList Int) @?= mml []
  , testCase "zip 3" $ zip' (mml [] :: MyList String) (mml [4]) @?= mml []
  , testCase "zip 4" $ zip' (mml ["Hi"]) (mml [4]) @?= mml [("Hi", 4)]
  , testCase "zip 5" $ zip' (mml [2, 3, 4]) (mml ["Hi", "Bye", "Why"]) @?=
      mml [(2, "Hi"), (3, "Bye"), (4, "Why")]
  , testCase "zip 6" $ zip' (mml [2, 3, 4, 5, 6]) (mml ["Hi", "Bye", "Why"]) @?=
      mml [(2, "Hi"), (3, "Bye"), (4, "Why")]
  , testCase "zip 7" $ zip' (mml [2, 3, 4]) (mml ["Hi", "Bye", "Why", "!"]) @?=
      mml [(2, "Hi"), (3, "Bye"), (4, "Why")]
  ]

enumerateTests :: TestTree
enumerateTests = testGroup "enumerate"
  [ testCase "enumerate 1" $ enumerate ([] :: [Int]) @?= []
  , testCase "enumerate 2" $ enumerate ["Hi"] @?= [(0, "Hi")]
  , testCase "enumerate 3" $ enumerate [5] @?= [(0, 5)]
  , testCase "enumerate 4" $ enumerate [0, 1, 5, 7, 10] @?=
      [(0, 0), (1, 1), (2, 5), (3, 7), (4, 10)]
  , testCase "enumerate 5" $ enumerate ["Hello", "World", "!"] @?=
      [(0, "Hello"), (1, "World"), (2, "!")]
  ]

calculateVelocitiesTests :: TestTree
calculateVelocitiesTests = testGroup "calculateVelocities"
  [ testCase "calculateVelocities 1" $ calculateVelocities [] @?= []
  , testCase "calculateVelocities 2" $ calculateVelocities [(1.0, 2.0)] @?= []
  , testCase "calculateVelocities 3" $ calculateVelocities [(1.0, 2.0), (2.0, 3.0)]
      `doubleListsClose` [1.0]
  , testCase "calculateVelocities 4" $ calculateVelocities
      [(1.0, 0.0), (2.0, 1.0), (5.0, 3.0), (4.0, 3.5)]
      `doubleListsClose` [1.0, 1.5, -2.0]
  , testCase "calculateVelocities 5" $ calculateVelocities
      [(1.0, 0.0), (2.0, 1.0), (5.0, 3.0), (4.0, 3.5), (6.0, 3.75), (20.0, 3.8), (19.5, 4.0)]
      `doubleListsClose` [1.0, 1.5, -2.0, 8.0, 280.0, -2.5]
  ]
  where
    doubleListsClose l1 l2 = do
      let result = all (\(d1,d2) -> abs (d2 - d1) < 0.000001) (zip l1 l2)
      assertBool ("Double lists are not the same: " <> show l1 <> " " <> show l2) result

palindromeTests :: TestTree
palindromeTests = testGroup "palindrome"
  [ testCase "palindrome 1" $ palindrome "" @?= True
  , testCase "palindrome 2" $ palindrome "a" @?= True
  , testCase "palindrome 3" $ palindrome "ab" @?= False
  , testCase "palindrome 4" $ palindrome "abab" @?= False
  , testCase "palindrome 5" $ palindrome "abba" @?= True
  , testCase "palindrome 6" $ palindrome "a bba" @?= True
  , testCase "palindrome 7" $ palindrome "a man a plan a canal panama " @?= True
  , testCase "palindrome 8" $ palindrome "a man a can a canal panama " @?= False
  , testCase "palindrome 9" $ palindrome "madam im adam" @?= True
  , testCase "palindrome 10" $ palindrome "madam i'm adam" @?= False
  ]

makeDigitsTests :: TestTree
makeDigitsTests = testGroup "makeDigits"
  [ testCase "makeDigits 1" $ makeDigits 0 @?= []
  , testCase "makeDigits 2" $ makeDigits 5 @?= [5]
  , testCase "makeDigits 3" $ makeDigits 16 @?= [1, 6]
  , testCase "makeDigits 4" $ makeDigits 3042 @?= [3, 0, 4, 2]
  ]

allFactorsTests :: TestTree
allFactorsTests = testGroup "allFactors"
  [ testCase "allFactors 1" $ allFactors [] @?= []
  , testCase "allFactors 2" $ allFactors [1] @?= [1]
  , testCase "allFactors 3" $ allFactors [1, 2] @?= [1, 2]
  , testCase "allFactors 4" $ allFactors [2] @?= [1, 2]
  , testCase "allFactors 5" $ allFactors [3, 1] @?= [1, 3]
  , testCase "allFactors 6" $ allFactors [8, 4, 9] @?= [1, 2, 3, 4, 8, 9]
  , testCase "allFactors 7" $ allFactors [13, 9, 64, 16, 100] @?= [1, 2, 3, 4, 5, 8, 9, 10, 13, 16, 20, 25, 32, 50, 64, 100]
  ]

canStartShedTests :: TestTree
canStartShedTests = testGroup "canStartShed"
  [ testCase "canStartShed 1" $ canStartShed 5 2 [] @?= False
  , testCase "canStartShed 2" $ canStartShed 5 0 [] @?= True
  , testCase "canStartShed 3" $ canStartShed 5 2 [5, 10, 12, 11, 20] @?= False
  , testCase "canStartShed 4" $ canStartShed 5 2 [5, 10, 12, 11, 20, 3] @?= True
  , testCase "canStartShed 5" $ canStartShed 0 4 [-3, -2, -1, 0, 1, 2, 3, 4] @?= True
  , testCase "canStartShed 6" $ canStartShed 0 4 [-2, -1, 0, 1, 2, 3, 4] @?= False
  , testCase "canStartShed 7" $ canStartShed 14 3 [12, 9, 15, 18, 20, 5] @?= True
  , testCase "canStartShed 8" $ canStartShed 10 4 [12, 9, 15, 18, 20, 5] @?= False
  ]
