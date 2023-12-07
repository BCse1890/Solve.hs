module Main where

import Data.Char (toLower, toUpper)
import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture10
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 10 Tests"
  [ evolveStateTests
  , evolveUntilTests
  , runFSMTests
  , runPDATests
  , treeGrowthTests
  , fishPopulationTests
  , countCarsTests
  , playPacmanTests
  , solveRegexTests
  , solveCFGTests
  ]

evolveStateTests :: TestTree
evolveStateTests = testGroup "evolveState"
  [ testCase "evolveState 1" $ evolveState (+ 2) 5 3 @?= 11
  , testCase "evolveState 2" $ evolveState (* 2) 1 8 @?= 256
  , testCase "evolveState 3" $ evolveState ('a' :) "ba" 4 @?= "aaaaba"
  , testCase "evolveState 4" $ evolveState (++ [1, 2]) [4, 5, 6] 3 @?= [4, 5, 6, 1, 2, 1, 2, 1, 2]
  ]

evolveUntilTests :: TestTree
evolveUntilTests = testGroup "evolveUntil"
  [ testCase "evolveUntil 1" $ evolveUntil (+ 2) 5 (> 20) @?= 21
  , testCase "evolveUntil 2" $ evolveUntil ('a' :) "ba" ((> 10) . length) @?= "aaaaaaaaaba"
  , testCase "evolveUntil 3" $ evolveUntil f [] ((> 50) . sum) @?= [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  , testCase "evolveUntil 4" $ evolveUntil f [1] ((> 40) . sum) @?= [9, 8, 7, 6, 5, 4, 3, 2, 1]
  , testCase "evolveUntil 5" $ evolveUntil f [10, 20] ((> 100) . sum) @?=
      [16, 15, 14, 13, 12, 11, 10, 20]
  ]
  where
    f [] = [2, 1]
    f (a : rest) = (a + 2 : a + 1 : a : rest)

data SampleFSM1 = HasSeenA | NotSeenA
  deriving (Show, Eq, Enum)

sample1 :: Char -> SampleFSM1 -> SampleFSM1
sample1 _ HasSeenA = HasSeenA
sample1 'a' _ = HasSeenA
sample1 _ _ = NotSeenA

data SampleFSM2 = Baseline | JustA | JustAB
  deriving (Show, Eq, Enum)

sample2 :: Char -> SampleFSM2 -> SampleFSM2
sample2 'a' _ = JustA
sample2 'b' JustA = JustAB
sample2 _ _ = Baseline

runFSMTests :: TestTree
runFSMTests = testGroup "runFSM"
  [ testCase "runFSM 1" $ runFSM sample1 (== HasSeenA) NotSeenA "" @?= False
  , testCase "runFSM 2" $ runFSM sample1 (== HasSeenA) NotSeenA "bcdefghijklmnop" @?= False
  , testCase "runFSM 3" $ runFSM sample1 (== HasSeenA) NotSeenA "a" @?= True
  , testCase "runFSM 4" $ runFSM sample1 (== HasSeenA) NotSeenA "bmnaksjdjgoa" @?= True
  , testCase "runFSM 5" $ runFSM sample1 (== NotSeenA) NotSeenA "" @?= True
  , testCase "runFSM 6" $ runFSM sample1 (== NotSeenA) NotSeenA "bcdefghijklmnop" @?= True
  , testCase "runFSM 7" $ runFSM sample1 (== NotSeenA) NotSeenA "a" @?= False
  , testCase "runFSM 8" $ runFSM sample1 (== NotSeenA) NotSeenA "bmnaksjdjgoa" @?= False
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "bmnaksjdjgoa" @?= False
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "bmnaksjdjgoab" @?= True
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "" @?= False
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "a" @?= False
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "b" @?= False
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "ab" @?= True
  , testCase "runFSM 8" $ runFSM sample2 (== JustAB) Baseline "abc" @?= False
  ]

data SampleCFG = CountXs | CountYs | SampleSuccess | SampleFailure | SampleInitial
  deriving (Show, Eq, Enum)

sample3 :: Char -> (SampleCFG, [Char]) -> (SampleCFG, [Char])
sample3 c (st, stack) = case (st, stack, c) of
  (SampleInitial, [], 'x') -> (CountXs, ['x'])
  (SampleFailure, _, _) -> (SampleFailure, stack)
  (SampleSuccess, _, _) -> (SampleFailure, stack)
  (CountXs, _, 'x') -> (CountXs, 'x' : stack)
  (CountXs, ['x'], 'y') -> (SampleSuccess, [])
  (CountXs, ('x' : rest), 'y') -> (CountYs, rest)
  (CountYs, ['x'], 'y') -> (SampleSuccess, [])
  (CountYs, ('x' : rest), 'y') -> (CountYs, rest)
  _ -> (SampleFailure, stack)

runPDATests :: TestTree
runPDATests = testGroup "runPDA"
  [ testCase "runPDA 1" $ runPDA sample3 f SampleInitial "" @?= True
  , testCase "runPDA 2" $ runPDA sample3 f SampleInitial "x" @?= False
  , testCase "runPDA 3" $ runPDA sample3 f SampleInitial "y" @?= False
  , testCase "runPDA 4" $ runPDA sample3 f SampleInitial "xy" @?= True
  , testCase "runPDA 5" $ runPDA sample3 f SampleInitial "xxy" @?= False
  , testCase "runPDA 6" $ runPDA sample3 f SampleInitial "xyy" @?= False
  , testCase "runPDA 7" $ runPDA sample3 f SampleInitial "xyxy" @?= False
  , testCase "runPDA 8" $ runPDA sample3 f SampleInitial "xxyy" @?= True
  , testCase "runPDA 9" $ runPDA sample3 f SampleInitial "xxxxxyyyyy" @?= True
  ]
  where
    f s = s == SampleInitial || s == SampleSuccess

treeGrowthTests :: TestTree
treeGrowthTests = testGroup "treeGrowth"
  [ testCase "treeGrowth 1" $ treeGrowth 2 0 @?= 2
  , testCase "treeGrowth 2" $ treeGrowth 2 1 @?= 7
  , testCase "treeGrowth 3" $ treeGrowth 2 3 @?= 37
  , testCase "treeGrowth 4" $ treeGrowth 5 5 @?= 253
  , testCase "treeGrowth 5" $ treeGrowth 1 10 @?= 4093
  ]

fishPopulationTests :: TestTree
fishPopulationTests = testGroup "fishPopulation"
  [ testCase "fishPopulation 1" $ fishPopulation 0 100 @?= 1000
  , testCase "fishPopulation 2" $ fishPopulation 1 100 @?= 1000
  , testCase "fishPopulation 3" $ fishPopulation 3 100 @?= 1000
  , testCase "fishPopulation 4" $ fishPopulation 3 50 @?= 1165
  , testCase "fishPopulation 5" $ fishPopulation 3 150 @?= 834
  , testCase "fishPopulation 6" $ fishPopulation 100 99 @?= 86328
  ]

countCarsTests :: TestTree
countCarsTests = testGroup "countCars"
  [ testCase "countCars 1" $ countCars (10, 10) ([10], [10], [10], [10]) 60 @?= 40
  , testCase "countCars 2" $ countCars (60, 10) ([10], [10], [10], [10]) 60 @?= 20
  , testCase "countCars 3" $ countCars (60, 10) ([10, 5, 8], [10, 5, 7], [10], [10]) 60 @?= 45
  , testCase "countCars 4" $ countCars (50, 10) ([10, 5, 8], [10, 5, 7], [10], [10]) 60 @?= 63
  , testCase "countCars 5" $ countCars (5, 5)
      ([10], [10], [10], [10]) 5 @?= 8
  , testCase "countCars 6" $ countCars (5, 5)
      ([10], [10], [10], [10]) 6 @?= 8
  , testCase "countCars 7" $ countCars (5, 5)
      ([10], [10], [10], [10]) 7 @?= 10
  , testCase "countCars 8" $ countCars (5, 5)
      ([10], [10], [10], [10]) 11 @?= 16
  , testCase "countCars 9" $ countCars (5, 5)
      ([10], [10], [10], [10]) 12 @?= 18
  , testCase "countCars 10" $ countCars (5, 8)
      ([4, 10], [2, 10], [10], [10]) 10 @?= 14
  , testCase "countCars 11" $ countCars (5, 8)
      ([4, 10], [2, 10], [10], [10]) 13 @?= 20
  , testCase "countCars 12" $ countCars (5, 8)
      ([4, 10], [2, 10], [10], [10]) 26 @?= 32
  ]

playPacmanTests :: TestTree
playPacmanTests = testGroup "playPacman"
  [ testCase "playPacman 1" $ playPacman ((3, 8), (8, 3), (7,9))  @?= False
  , testCase "playPacman 2" $ playPacman ((0,4), (4,0), (8,7)) @?= True
  , testCase "playPacman 3" $ playPacman ((3,8), (2,0), (7,7)) @?= True
  , testCase "playPacman 4" $ playPacman ((0,1), (0,8), (7,7)) @?= True
  , testCase "playPacman 5" $ playPacman ((8,8), (6,8), (7,9)) @?= False
  ]

solveRegexTests :: TestTree
solveRegexTests = testGroup "solveRegex"
  [ testCase "solveRegex 1" $ solveRegex "" @?= False
  , testCase "solveRegex 2" $ solveRegex "abcdefg" @?= True
  , testCase "solveRegex 3" $ solveRegex "abcdef" @?= True
  , testCase "solveRegex 4" $ solveRegex "abcde" @?= True
  , testCase "solveRegex 5" $ solveRegex "abcdegfgegfegf" @?= True
  , testCase "solveRegex 6" $ solveRegex "abcd" @?= True
  , testCase "solveRegex 7" $ solveRegex "abc" @?= False
  , testCase "solveRegex 8" $ solveRegex "abcdefa" @?= False
  , testCase "solveRegex 9" $ solveRegex "abcefg" @?= False
  , testCase "solveRegex 10" $ solveRegex "defg" @?= False
  , testCase "solveRegex 11" $ solveRegex "abcabcabcd" @?= True
  , testCase "solveRegex 12" $ solveRegex "abcabcabcdefggfe" @?= True
  ]

solveCFGTests :: TestTree
solveCFGTests = testGroup "solveCFG"
  [ testCase "solveCFG 1" $ solveCFG "" @?= True
  , testCase "solveCFG 2" $ solveCFG "a" @?= False
  , testCase "solveCFG 3" $ solveCFG "ab" @?= False
  , testCase "solveCFG 4" $ solveCFG "abc" @?= False
  , testCase "solveCFG 5" $ solveCFG "abcd" @?= True
  , testCase "solveCFG 6" $ solveCFG "bcd" @?= False
  , testCase "solveCFG 7" $ solveCFG "abbbcccd" @?= True
  , testCase "solveCFG 8" $ solveCFG "aabbbcccdd" @?= True
  , testCase "solveCFG 9" $ solveCFG "aabcdd" @?= True
  , testCase "solveCFG 10" $ solveCFG "ad" @?= True
  , testCase "solveCFG 11" $ solveCFG "aadd" @?= True
  , testCase "solveCFG 12" $ solveCFG "bc" @?= True
  , testCase "solveCFG 13" $ solveCFG "bbbccc" @?= True
  , testCase "solveCFG 14" $ solveCFG "bbbadccc" @?= False
  , testCase "solveCFG 15" $ solveCFG "aabbcd" @?= False
  ]
