module Main where

import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Data.Char (toLower, toUpper)
import Test.Tasty
import Test.Tasty.HUnit

import M1Lecture11
import MyList
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 11 Tests"
  [ countIntervalsExcludingSpikesTests
  , backpackMealsTests
  , stackBoxesTests
  , runAssemblyTests
  , wordSearchTests
  , hiddenMessageTests
  ]

countIntervalsExcludingSpikesTests :: TestTree
countIntervalsExcludingSpikesTests = testGroup "countIntervalsExcludingSpikes"
  [ testCase "countIntervalsExcludingSpikes 1" $ shouldReturnLogger (countIntervalsExcludingSpikes [] []) 0
  , testCase "countIntervalsExcludingSpikes 2" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1,3), (5,8), (6,10)] []) 9
  , testCase "countIntervalsExcludingSpikes 3" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1,3), (5,8), (6,10)] [8, 6, 12]) 7
  , testCase "countIntervalsExcludingSpikes 4" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1,3), (5,8), (6,10)] [3, 2, 1]) 6
  , testCase "countIntervalsExcludingSpikes 5" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1,3), (5,8), (6,10)] [1, 2, 3, 5, 4, 6, 8, 7, 9, 10]) 0
  , testCase "countIntervalsExcludingSpikes 6" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1,3), (5,8), (6,10)] [1, 2, 3, 4, 5, 6, 7, 8, 9]) 1
  , testCase "countIntervalsExcludingSpikes 7" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1, 1000000), (1000005, 2000000)] []) 1999996
  , testCase "countIntervalsExcludingSpikes 8" $ shouldReturnLogger (countIntervalsExcludingSpikes [(1, 1000000), (1000005, 2000000)] [323, 999999, 1000002, 1999000]) 1999993
  ]

backpackMealsTests :: TestTree
backpackMealsTests = testGroup "backpackMeals"
  [ testCase "backpackMeals 1" $ shouldReturnLogger (backpackMeals 11 [4,2] [6, 9, 2]) 11
  , testCase "backpackMeals 2" $ shouldReturnLogger (backpackMeals 1 [] []) 0
  , testCase "backpackMeals 2" $ shouldReturnLogger (backpackMeals 11 [] [6, 9, 2]) 0
  , testCase "backpackMeals 3" $ shouldReturnLogger (backpackMeals 11 [4,2] []) 0
  , testCase "backpackMeals 4" $ shouldReturnLogger (backpackMeals 11 [4,2] [12]) 0
  , testCase "backpackMeals 5" $ shouldReturnLogger (backpackMeals 9 [4,2] [6, 9, 2]) 8
  , testCase "backpackMeals 6" $
      shouldReturnLogger (backpackMeals 24 [15, 3, 1, 12, 25, 4,2] [7, 11, 14, 3]) 23
  ]

stackBoxesTests :: TestTree
stackBoxesTests = testGroup "stackBoxes"
  [ testCase "stackBoxes 1" $ shouldReturnLogger (stackBoxes ["abc", ""] [(0,1,3)]) "cab"
  , testCase "stackBoxes 2" $ shouldReturnLogger (stackBoxes ["abc", "d"] [(0,1,3)]) "cabd"
  , testCase "stackBoxes 3" $ shouldReturnLogger (stackBoxes ["abcz", "def"] [(1,0,1), (0,1,5)]) "zbcdaef"
  , testCase "stackBoxes 4" $ shouldReturnLogger
      (stackBoxes ["abc", "def", "", "ghi"] [(3,2,3), (1,2,1), (1,3,2), (0,1,2), (2,0,1)]) "dcabighef"
  , testCase "stackBoxes 5" $ shouldReturnLogger
      (stackBoxes ["abc", "", "ph"] [(0,1,3), (2,0,1)]) "pcabh"
  , testCase "stackBoxes 6" $ shouldReturnLogger
      (stackBoxes ["abc", "", "defgh", "ij"] [(0,1,1), (2,1,3), (3,2,2)]) "bcfdeaijgh"
  ]

runAssemblyTests :: TestTree
runAssemblyTests = testGroup "runAssembly"
  [ testCase "runAssembly 1" $ shouldReturnLogger
      (runAssembly [LoadValue Eax 4, PrintRegister Eax]) [4]
  , testCase "runAssembly 2" $ shouldReturnLogger
      (runAssembly [LoadValue Eax 4, LoadValue Ebx (-3), AddRegister Eax Ebx, PrintRegister Eax])
      [1]
  , testCase "runAssembly 3" $ shouldReturnLogger
      (runAssembly
        [ LoadValue Eax 4
        , LoadValue Ebx 2
        , LoadValue Ecx 7
        , AddRegister Ebx Ecx
        , SubRegister Eax Ebx -- (-5, 9, 7)
        , PrintRegister Eax
        , PrintRegister Ebx
        , SubRegister Eax Eax
        , AddRegister Eax Ecx -- (7, 9, 7)
        , MultRegister Ebx Eax -- (7, 63, 7)
        , PrintRegister Ebx
        , PrintRegister Ecx
        , LoadValue Eax 3
        , SubRegister Ecx Eax
        , PrintRegister Ecx
        ]
      )
      [-5, 9, 63, 7, 4]
  ]

wordSearchTests :: TestTree
wordSearchTests = testGroup "wordSearch"
  [ testCase "wordSearch 1" $ shouldReturnLogger (wordSearch "hop" g1) False
  , testCase "wordSearch 2" $ shouldReturnLogger (wordSearch "f" g1) False
  , testCase "wordSearch 3" $ shouldReturnLogger (wordSearch "z" g1) True
  , testCase "wordSearch 4" $ shouldReturnLogger (wordSearch "pry" g1) True
  , testCase "wordSearch 5" $ shouldReturnLogger (wordSearch "apple" g1) True
  , testCase "wordSearch 6" $ shouldReturnLogger (wordSearch "egg" g1) True
  , testCase "wordSearch 7" $ shouldReturnLogger (wordSearch "hip" g1) True
  , testCase "wordSearch 8" $ shouldReturnLogger (wordSearch "qfad" g2) True
  , testCase "wordSearch 9" $ shouldReturnLogger (wordSearch "edkap" g2) True
  , testCase "wordSearch 10" $ shouldReturnLogger ( wordSearch "pqiej" g2) True
  , testCase "wordSearch 11" $ shouldReturnLogger ( wordSearch "enfm" g2) True
  ]
  where
    g1 = ["aghip", "gpgor", "zupey", "mcslq", "hevle"]
    g2 = ["pdkej", "maneq", "pficx", "mqjcj", "pakde"]

hiddenMessageTests :: TestTree
hiddenMessageTests = testGroup "hiddenMessage"
  [ testCase "hiddenMessage 1" $ shouldReturnLogger (hiddenMessage ["win"]) "win"
  , testCase "hiddenMessage 2" $ shouldReturnLogger (hiddenMessage ["wn", "wi", "in"]) "win"
  , testCase "hiddenMessage 3" $ shouldReturnLogger (hiddenMessage ["wi", "in", "nd"]) "wind"
  , testCase "hiddenMessage 4" $ shouldReturnLogger (hiddenMessage ["wid", "ind"]) "wind"
  , testCase "hiddenMessage 5" $ shouldReturnLogger (hiddenMessage ["wne", "witr", "ine", "int", "ter"]) "winter"
  , testCase "hiddenMessage 6" $ shouldReturnLogger (hiddenMessage ["lo", "ob", "bs", "st", "te", "er"]) "lobster"
  , testCase "hiddenMessage 7" $ shouldReturnLogger (hiddenMessage ["loer", "lbte", "obsr", "ster"]) "lobster"
  ]
