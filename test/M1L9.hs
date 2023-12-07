{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.State ()
import Data.Char (toLower, toUpper)
import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture9
import Utils
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 9 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ mapMTests
  , foldMTests
  , sequenceTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ commonTokenTests
  , compartmentalizeTests
  , anyOverlapTests
  , traverse2DTests
  , queueTests
  , math2dTests
  ]

increment :: Int -> State Int Int
increment x = do
  current <- get
  put (current + x)
  return (current + x)

multiplyAndIncrement :: Int -> Int -> State Int Int
multiplyAndIncrement y x = increment (y * x)

logStr :: String -> Writer String Char
logStr s = do
  tell s
  tell " "
  return (head s)

snocAndLogStr :: Char -> String -> Writer String Char
snocAndLogStr c s = do
  tell (s ++ [c])
  tell " "
  return (head s)

mapMTests :: TestTree
mapMTests = testGroup "mapM"
  [ testCase "mapM 1" $ evalState (mapM' increment Nil) 10 @?= Nil
  , testCase "mapM 2" $ evalState (mapM' increment triple) 10 @?= mml [11, 13, 16]
  , testCase "mapM 3" $ evalState (mapM' increment (mml [15, 20, 30])) 1 @?= mml [16, 36, 66]
  , testCase "mapM 3" $ runWriter (mapM' logStr many) @?= (mml "HW!G", "Hello World ! Goodbye ")
  ]

foldMTests :: TestTree
foldMTests = testGroup "foldM"
  [ testCase "foldM 1" $ evalState (foldM' multiplyAndIncrement 0 Nil) 0 @?= 0
  , testCase "foldM 2" $ evalState (foldM' multiplyAndIncrement 0 single) 0 @?= 0
  , testCase "foldM 3" $ evalState (foldM' multiplyAndIncrement 1 single) 0 @?= 1
  , testCase "foldM 4" $ evalState (foldM' multiplyAndIncrement 1 double) 0 @?= 3
  , testCase "foldM 5" $ evalState (foldM' multiplyAndIncrement 1 triple) 0 @?= 12
  , testCase "foldM 6" $ evalState (foldM' multiplyAndIncrement 1 triple) 5 @?= 72
  , testCase "foldM 7" $ runWriter (foldM' snocAndLogStr 'a' many) @?=
      ('G', "Helloa WorldH !W Goodbye! ")
  ]

sequenceTests :: TestTree
sequenceTests = testGroup "sequence"
  [ testCase "sequence 1" $ evalState (sequence' Nil) 0 @?= (Nil :: MyList Int)
  , testCase "sequence 2" $ evalState (sequence' (map' increment single)) 0 @?= single
  , testCase "sequence 3" $ evalState (sequence' (map' increment triple)) 0 @?= mml [1, 3, 6]
  , testCase "sequence 4" $ evalState (sequence' (map' increment triple)) 10 @?= mml [11, 13, 16]
  , testCase "sequence 5" $ runWriter (sequence' (map' logStr many)) @?=
      (mml "HW!G", "Hello World ! Goodbye ")

  ]

commonTokenTests :: TestTree
commonTokenTests = testGroup "commonToken"
  [ testCase "commonToken 1" $ commonToken [] `shouldReturnIO` ""
  , testCase "commonToken 2" $ commonToken [] @?= Just ""
  , testCase "commonToken 3" $ commonToken ["ab", "ac", "ad"] `shouldReturnIO` "a"
  , testCase "commonToken 4" $ commonToken ["ab", "bc", "ba"] `shouldReturnIO` "b"
  , testCase "commonToken 5" $ commonToken
      ["hallway", "wink", "wolf", "iced", "inked", "it", "new", "network", "nut"] @?= Just "win"
  , testCase "commonToken 6" $ commonToken
      [ "input", "reject", "tornado", "realize", "criminal", "isotope"
      , "left", "egregious", "rife"] @?= Just "tie"
  , testCase "commonToken 7" $ commonToken
      [ "input", "reject", "tornado", "realize", "criminal", "isotope"
      , "left", "egregious", "rife", "eye"] @?= Nothing
  , testCase "commonToken 8" $ commonToken
      [ "input", "reject", "tornado", "realize", "criminal", "isotope"
      , "left", "egregious", "rif"] @?= Nothing
  , testCase "commonToken 9" $ commonToken
      [ "input", "reject", "tornado", "realize", "criminal", "aisotope"
      , "left", "egregious", "rife"] @?= Nothing
  ]

compartmentalizeTests :: TestTree
compartmentalizeTests = testGroup "compartmentalize"
  [ testCase "compartmentalize 1" $ compartmentalize "abad" 2 @?= Just 'a'
  , testCase "compartmentalize 2" $ compartmentalize "abadcdup" 2 @?= Just 'd'
  , testCase "compartmentalize 3" $ compartmentalize "abadcdua" 2 @?= Nothing
  , testCase "compartmentalize 4" $ compartmentalize "abcdefgh" 2 @?= Nothing
  , testCase "compartmentalize 5" $ compartmentalize "hilhugmuh" 3 @?= Just 'h'
  , testCase "compartmentalize 6" $ compartmentalize "hilhugmuh" 3 `shouldReturnIO` 'h'
  , testCase "compartmentalize 7" $ compartmentalize "ibegpgdeniemquem" 4 @?= Just 'e'
  , testCase "compartmentalize 8" $ compartmentalize "ibegpgdeniemquem" 4
      `shouldReturnIO` 'e'
  ]

anyOverlapTests :: TestTree
anyOverlapTests = testGroup "anyOverlap"
  [ testCase "anyOverlap 1" $ testLogger (anyOverlap []) False ["No overlap found!"]
  , testCase "anyOverlap 2" $ testLogger (anyOverlap [(1, 5)]) False ["No overlap found!"]
  , testCase "anyOverlap 3" $ testLogger (anyOverlap [(1, 5), (5, 10)]) True
      ["(1,5) and (5,10) overlap!"]
  , testCase "anyOverlap 4" $ testLogger (anyOverlap [(5, 10), (1, 5)]) True
      ["(1,5) and (5,10) overlap!"]
  , testCase "anyOverlap 5" $ testLogger (anyOverlap [(1, 6), (4, 10)]) True
      ["(1,6) and (4,10) overlap!"]
  , testCase "anyOverlap 6" $ testLogger (anyOverlap [(4, 10), (1, 6)]) True
      ["(1,6) and (4,10) overlap!"]
  , testCase "anyOverlap 7" $ testLogger (anyOverlap [(4, 8), (9, 15)]) False
      ["(4,8) and (9,15) do not overlap!", "No overlap found!"]
  , testCase "anyOverlap 8" $ testLogger
      (anyOverlap [(4, 8), (9, 15), (20, 30), (31, 40), (41, 50)]) False
      [ "(4,8) and (9,15) do not overlap!"
      , "(9,15) and (20,30) do not overlap!"
      , "(20,30) and (31,40) do not overlap!"
      , "(31,40) and (41,50) do not overlap!"
      , "No overlap found!"
      ]
  , testCase "anyOverlap 9" $ testLogger
      (anyOverlap [(4, 8), (9, 15), (20, 30), (31, 40), (1, 6)]) True
      ["(1,6) and (4,8) overlap!"]
  , testCase "anyOverlap 10" $ testLogger
      (anyOverlap [(4, 8), (9, 15), (20, 30), (31, 40), (19, 25)]) True
      [ "(4,8) and (9,15) do not overlap!"
      , "(9,15) and (19,25) do not overlap!"
      , "(19,25) and (20,30) overlap!"
      ]
  ]

traverse2DTests :: TestTree
traverse2DTests = testGroup "traverse2D"
  [ testCase "traverse2D 1" $ testLogger (traverse2D (0,0) []) 1 []
  , testCase "traverse2D 2" $ testLogger (traverse2D (2,3) [Up]) 2
      [mv (2,4)]
  , testCase "traverse2D 3" $ testLogger (traverse2D (0,0) [Up, Down, Left, Right]) 3
      (mv <$> [(0,1), (0,0), (-1,0), (0,0)])
  , testCase "traverse2D 4" $ testLogger (traverse2D (1,1) [Up, Right, Down, Left]) 4
      (mv <$> [(1,2), (2,2), (2,1), (1,1)])
  , testCase "traverse2D 5" $ testLogger (traverse2D (0,0) [Up, Right, Down, Down]) 5
      (mv <$> [(0,1), (1,1), (1,0), (1,-1)])
  , testCase "traverse2D 6" $ testLogger (traverse2D (1,1)
      [Up,Right,Down,Left,Up,Left,Down,Right,Up,Up]) 7
      (mv <$> [(1,2), (2,2), (2,1), (1,1), (1,2), (0,2), (0,1), (1,1), (1,2), (1,3)])
  ]
  where
    mv t = "Visiting: " <> show t

queueTests :: TestTree
queueTests = testGroup "queue"
  [ testCase "queue 1" $ results @?=
      [Nothing, Just 5, Nothing, Just 6, Just 7, Just 8, Just 9, Nothing]
  ]
  where
    action :: State (ListQueue Int) [Maybe Int]
    action = do
      z <- dequeue
      enqueue 5
      a <- dequeue
      b <- dequeue
      enqueue 6
      enqueue 7
      enqueue 8
      c <- dequeue
      d <- dequeue
      enqueue 9
      e <- dequeue
      f <- dequeue 
      g <- dequeue
      return [z, a, b, c, d, e, f, g]

    results :: [Maybe Int]
    results = evalState action (ListQueue ([] :: [Int]) [])

math2dTests :: TestTree
math2dTests = testGroup "math2d"
  [ testCase "math2d 1" $ testLogger' (math2d []) (Just 0) []
  , testCase "math2d 1" $ testLogger' (math2d [[1,2,3]]) (Just 5)
      ["After row 1, value is 5"]
  , testCase "math2d 3" $ testLogger' (math2d [[1, 2, 3, 4]]) (Just 20)
      ["After row 1, value is 20"]
  , testCase "math2d 4" $ testLogger' (math2d [[1, 2, 3, 4, 5]]) (Just 25)
      ["After row 1, value is 25"]
  , testCase "math2d 5" $ testLogger' (math2d [[1, -2, 3, 4, -5]]) (Just (-1))
      ["After row 1, value is -1"]
  , testCase "math2d 6" $ testLogger' (math2d [[1, 2, 3, 4],[5, 6, 7, 8]]) (Just 1256)
      ["After row 1, value is 20", "After row 2, value is 1256"]
  , testCase "math2d 7" $ testLogger' (math2d [[1, 2, 3, 4, 5],[5, 6, 7, 8, 9]]) (Just 1505)
      ["After row 1, value is 25", "After row 2, value is 1505"]
  , testCase "math2d 8" $ testLogger' (math2d [[1, 2, 3], [-4, 5, 3], [2, 2, 2]]) (Just 22)
      ["After row 1, value is 5", "After row 2, value is 8", "After row 3, value is 22"]
  , testCase "math2d 9" $ testLogger' (math2d [[4,3,-2], [6,2,13]]) (Just 45)
      ["After row 1, value is 10", "After row 2, value is 45"]
  ]
