module Main where

import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture1
import Utils
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 1 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ nullTests
  , headTests
  , tailTests
  , consTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ sum3Tests
  , coordTests
  , firstTravelTests
  ]

nullTests :: TestTree
nullTests = testGroup "Null"
  [ testCase "Null 1" $ null' Nil @?= True
  , testCase "Null 2" $ null' (Cons 1 Nil) @?= False
  , testCase "Null 3" $ null' (Cons 1 (Cons 2 (Cons 3 Nil))) @?= False
  ]

headTests :: TestTree
headTests = testGroup "Head"
  [ testCase "Head 1" $ head' (Cons 2 Nil) @?= 2
  , testCase "Head 2" $ head' (Cons 1 (Cons 2 Nil)) @?= 1
  ]

tailTests :: TestTree
tailTests = testGroup "Tail"
  [ testCase "Tail 1" $ tail' (Cons 2 Nil) @?= Nil
  , testCase "Tail 2" $ tail' (Cons 1 (Cons 2 Nil)) @?= Cons 2 Nil
  , testCase "Tail 3" $ tail' (Cons 1 (Cons 2 (Cons 3 Nil))) @?= Cons 2 (Cons 3 Nil)
  ]

consTests :: TestTree
consTests = testGroup "Cons"
  [ testCase "Cons 1" $ cons' 'a' Nil @?= Cons 'a' Nil
  , testCase "Cons 2" $ cons' 3 (Cons 1 Nil) @?= Cons 3 (Cons 1 Nil)
  , testCase "Cons 3" $ cons' 4 (Cons 1 (Cons 2 (Cons 3 Nil))) @?= Cons 4 (Cons 1 (Cons 2 (Cons 3 Nil)))
  ]

sum3Tests :: TestTree
sum3Tests = testGroup "sum3"
  [ testCase "sum3 1" $ sum3 [] @?= 0
  , testCase "sum3 2" $ sum3 [4] @?= 4
  , testCase "sum3 3" $ sum3 [4, -2] @?= 2
  , testCase "sum3 4" $ sum3 [4, 5] @?= 9
  , testCase "sum3 5" $ sum3 [4, 5, 6] @?= 15
  , testCase "sum3 6" $ sum3 [4, -2, -6] @?= (-4)
  , testCase "sum3 7" $ sum3 [4, 5, 7, 6, 3, -1] @?= 16
  , testCase "sum3 8" $ sum3 [12, -2, -6, -3, -5, 2] @?= 4
  ]

coordTests :: TestTree
coordTests = testGroup "Coordinates and Directions"
  [ testCase "Coord/Dir 1" $ moveD4 4 (1, 2) Up @?= (1, 6)
  , testCase "Coord/Dir 2" $ moveD4 4 (1, 2) Right @?= (5, 2)
  , testCase "Coord/Dir 3" $ moveD4 3 (6, 1) Down @?= (6, -2)
  , testCase "Coord/Dir 4" $ moveD4 10 (6, 1) Left @?= (-4, 1)
  , testCase "Coord/Dir 5" $ stepD4 (1, 2) Up @?= (1, 3)
  , testCase "Coord/Dir 6" $ stepD4 (1, 2) Right @?= (2, 2)
  , testCase "Coord/Dir 7" $ stepD4 (6, 1) Down @?= (6, 0)
  , testCase "Coord/Dir 8" $ stepD4 (6, 1) Left @?= (5, 1)
  , testCase "Coord/Dir 9" $ moveD4f 4.5 (1.5, 2.25) Up @?~= (1.5, 6.75)
  , testCase "Coord/Dir 10" $ moveD4f 4.1 (1.3, 2) Right @?~= (5.4, 2)
  , testCase "Coord/Dir 11" $ moveD4f 3 (6, 0.5) Down @?~= (6, -2.5)
  , testCase "Coord/Dir 12" $ moveD4f 10.3 (6.7, 1) Left @?~= (-3.6, 1)
  , testCase "Coord/Dir 13" $ stepD4f (1.3, 2.3) Up @?~= (1.3, 3.3)
  , testCase "Coord/Dir 14" $ stepD4f (1.1, 2.1) Right @?~= (2.1, 2.1)
  , testCase "Coord/Dir 15" $ stepD4f (6, 0.9) Down @?~= (6, -0.1)
  , testCase "Coord/Dir 16" $ stepD4f (6.74, 1) Left @?~= (5.74, 1)
  , testCase "Coord/Dir 17" $ moveD4' (Movement4 Up 4) (1, 2) @?= (1, 6)
  , testCase "Coord/Dir 18" $ moveD4' (Movement4 Right 4) (1, 2) @?= (5, 2)
  , testCase "Coord/Dir 19" $ moveD4' (Movement4 Down 3) (6, 1) @?= (6, -2)
  , testCase "Coord/Dir 20" $ moveD4' (Movement4 Left 10) (6, 1) @?= (-4, 1)
  ]

firstTravelTests :: TestTree
firstTravelTests = testGroup "firstTravel"
  [ testCase "firstTravel 1" $ firstTravel [] @?= 0
  , testCase "firstTravel 2" $ firstTravel [(2, 5)] @?= 0
  , testCase "firstTravel 3" $ firstTravel [(2, 5), (4, -9)] @?= 16
  , testCase "firstTravel 4" $ firstTravel [(1, 3), (6, 10)] @?= 12
  , testCase "firstTravel 5" $ firstTravel [(1, 3), (6, 10), (7, 11), (8, 15)] @?= 12
  ]
