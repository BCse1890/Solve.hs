{-# LANGUAGE TupleSections #-}

module Main where

import Data.Maybe (fromJust)
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import M2Lecture3
import Utils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 3 Tests"
  [ occMapTests
  , calculationGraphTests
  , pickBestClumpTests
  , directoryCountsTests
  , algaeGrowthTests
  , sparseMatrixTests
  ]

occMapTests :: TestTree
occMapTests = testGroup "occMap"
  [ testCase "occMap addKey 1" $ addKey M.empty "Hello" 5 @?= m1
  , testCase "occMap addKey 2" $ addKey m1 "Hello" 5 @?= M.fromList [("Hello", 10)]
  , testCase "occMap addKey 3" $ addKey m1 "Goodbye" 3 @?= m2
  , testCase "occMap incKey 1" $ incKey m1 "Goodbye" @?=
      M.fromList [("Hello", 5), ("Goodbye", 1)]
  , testCase "occMap incKey 2" $ incKey m2 "Goodbye" @?=
      M.fromList [("Hello", 5), ("Goodbye", 4)]
  , testCase "occMap incKey 3" $ incKey m2 "Hello" @?=
      M.fromList [("Hello", 6), ("Goodbye", 3)]
  , testCase "occMap incKey 4" $ incKey M.empty "Hello" @?= M.fromList [("Hello", 1)]
  , testCase "occMap subKey 1" $ subKey M.empty "Hello" 1 @?= M.empty
  , testCase "occMap subKey 2" $ subKey m1 "Hello" 5 @?= M.empty
  , testCase "occMap subKey 3" $ subKey m1 "Hello" 4 @?= M.fromList [("Hello", 1)]
  , testCase "occMap subKey 4" $ subKey m1 "Goodbye" 1 @?= m1
  , testCase "occMap subKey 5" $ subKey m2 "Goodbye" 2 @?=
      M.fromList [("Hello", 5), ("Goodbye", 1)]
  , testCase "occMap subKey 6" $ subKey m2 "Goodbye" 3 @?= m1
  , testCase "occMap subKey 7" $ subKey m2 "Goodbye" 4 @?= m1
  , testCase "occMap decKey 1" $ decKey M.empty "Hello" @?= M.empty
  , testCase "occMap decKey 2" $ decKey m1 "Hello" @?= M.fromList [("Hello", 4)]
  , testCase "occMap decKey 3" $ decKey m2 "Hello" @?=
      M.fromList [("Hello", 4), ("Goodbye", 3)]
  , testCase "occMap decKey 4" $ decKey (M.fromList [("Hello", 1)]) "Hello" @?= M.empty
  , testCase "occMap decKey 5" $ decKey (M.fromList [("Hello", 0)]) "Hello" @?= M.empty
  ]
  where
    m1 = M.fromList [("Hello", 5)]
    m2 = M.fromList [("Hello", 5), ("Goodbye", 3)]

calculationGraphTests :: TestTree
calculationGraphTests = testGroup "calculationGraph"
  [ testCase "calculationGraph 1" $ calculationGraph "a" [("a", Assign 5)] @?= 5
  , testCase "calculationGraph 2" $ calculationGraph "a"
      [("a", Dep "b" "c" Add),("b", Assign 6),("c", Assign 17)] @?= 23
  , testCase "calculationGraph 3" $ calculationGraph "a"
      [ ("a", Dep "b" "c" Mult),("b", Dep "e" "f" Add),("e", Dep "g" "h" Sub)
      , ("c", Assign 5), ("f", Assign (-3)), ("g", Assign 27), ("h", Assign 14)
      ] @?= 50
  , testCase "calculationGraph 4" $ calculationGraph "h"
      [ ("h", Dep "m" "n" Sub),("m", Dep "p" "q" Mult),("n", Dep "q" "r" Mult)
      , ("p", Dep "x" "y" Div), ("q", Dep "y" "z" Add), ("r", Dep "z" "w" Sub)
      , ("x", Assign 6), ("y", Assign 2), ("z", Assign 8), ("w", Assign 4)
      ] @?= (-10)
  , testCase "calculationGraph 5" $ calculationGraph "cat"
      [ ("cat", Dep "rat" "bat" Mult),("rat", Dep "dog" "log" Sub),("bat", Dep "fog" "cog" Add)
      , ("dog", Assign 15), ("log", Dep "ape" "cape" Add), ("fog", Dep "cape" "drape" Sub), ("cog", Dep "drape" "grape" Mult)
      , ("ape", Assign 12), ("cape", Dep "flow" "snow" Add), ("drape", Assign 4), ("grape", Dep "crow" "blow" Div)
      , ("flow", Dep "hoop" "loop" Div), ("snow", Assign 8), ("crow", Assign 55), ("blow", Dep "coop" "stoop" Add)
      , ("hoop", Assign 7), ("loop", Assign 2), ("coop", Assign 11), ("stoop", Assign 7)
      ] @?= (-152)
  ]

sparseMatrixTests :: TestTree
sparseMatrixTests = testGroup "sparseMatrix"
  [ testCase "sparseMatrix get/set 1" $ testGets m1
      [(0,0,1), (1,0,0), (0,1,1), (1,1,0)]
  , testCase "sparseMatrix get/set 2" $ testGets m2
      [(0,0,1), (1,0,1), (0,1,0), (1,1,0)]
  , testCase "sparseMatrix get/set 3" $ testGets m3
      [(2,3,12), (1,1,7), (0,0,1), (2,2,0),(3,0,0)]
  , testCase "sparseMatrix get/set 4" $ testGets m4
      [(0,1,4), (1,1,2), (2,2,1), (3,3,3),(2,0,0),(0,2,0)]
  , testCase "sparseMatrix get OOB 1" $ get' 2 0 m1 @?= Nothing
  , testCase "sparseMatrix get OOB 2" $ get' 0 0 m1 @?= Just 1
  , testCase "sparseMatrix get OOB 3" $ get' 2 2 m2 @?= Nothing
  , testCase "sparseMatrix get OOB 4" $ get' 0 4 m3 @?= Nothing
  , testCase "sparseMatrix add 1" $ testGets (add m1 m2)
      [(0,0,2),(0,1,1),(1,0,1),(1,1,0)]
  , testCase "sparseMatrix add 2" $ testGets (add m2 m1)
      [(0,0,2),(0,1,1),(1,0,1),(1,1,0)]
  , testCase "sparseMatrix add 3" $ testGets (add m3 m4)
      [(0,0,1),(0,1,4),(1,1,9),(2,2,1),(2,3,12),(3,3,3),(3,2,0),(1,0,0)]
  , testCase "sparseMatrix add 4" $ add' m1 m3 @?= Nothing
  , testCase "sparseMatrix add 5" $ testGets (fromJust (add' m1 m2))
      [(0,0,2),(0,1,1),(1,0,1),(1,1,0)]
  , testCase "sparseMatrix multiply 1" $ testGets (multiply m1 m2)
      [(0,0,2),(0,1,0),(1,0,0),(1,1,0)]
  , testCase "sparseMatrix multiply 2" $ testGets (multiply m2 m1)
      [(0,0,1),(0,1,1),(1,0,1),(1,1,1)]
  , testCase "sparseMatrix multiply 3" $ testGets (multiply m3 m4)
      [(0,0,0),(0,1,4),(1,1,14),(2,3,36),(3,3,0),(3,2,0),(2,2,0)]
  , testCase "sparseMatrix multiply 4" $ testGets (multiply m4 m3)
      [(0,0,0),(0,1,28),(1,1,14),(2,3,12),(3,3,0),(3,2,0),(2,2,0)]
  , testCase "sparseMatrix multiply 5" $ testGets (multiply m5 m6)
      [(0,0,1),(0,1,16),(1,0,28),(1,1,24)]
  , testCase "sparseMatrix multiply 6" $ testGets (multiply m6 m5)
      [(0,0,25),(0,1,2),(0,2,36),(1,0,20),(1,1,0),(1,2,30),(2,0,4),(2,1,8),(2,2,0)]
  , testCase "sparseMatrix multiply 7" $ testGets (fromJust $ multiply' m1 m2)
      [(0,0,2),(0,1,0),(1,0,0),(1,1,0)]
  , testCase "sparseMatrix multiply 8" $ multiply' m1 m3 @?= Nothing
  , testCase "sparseMatrix multiply 9" $ multiply' m3 m5 @?= Nothing
  ]
  where
    e22 = empty 2 2
    m1 = set 0 1 1 (set 0 0 1 e22)
    m2 = set 1 0 1 (set 0 0 1 e22)

    e44 = empty 4 4
    m3 = set 2 3 12 (set 1 1 7 (set 0 0 1 e44))
    m4 = set 3 3 3 (set 2 2 1 (set 1 1 2 (set 0 1 4 e44)))

    m5 = set 1 2 6 (set 1 0 4 (set 0 1 2 (set 0 0 1 (empty 2 3))))
    m6 = set 2 0 4 (set 1 1 5 (set 0 1 6 (set 0 0 1 (empty 3 2))))

    testGets m = mapM_ (\(r,c,v) -> get r c m @?= v) 

pickBestClumpTests :: TestTree
pickBestClumpTests = testGroup "pickBestClump"
  [ testCase "pickBestClump 1" $ pickBestClump [] @?= 0
  , testCase "pickBestClump 2" $ pickBestClump [1] @?= 2
  , testCase "pickBestClump 3" $ pickBestClump [1,1,1] @?= 6
  , testCase "pickBestClump 4" $ pickBestClump [1,2] @?= 4
  , testCase "pickBestClump 5" $ pickBestClump [1,1,2] @?= 6
  , testCase "pickBestClump 6" $ pickBestClump [2,3,4] @?= 9
  , testCase "pickBestClump 7" $ pickBestClump [2,3,3,4,4] @?= 16
  , testCase "pickBestClump 8" $ pickBestClump [5,6,8,8,2,3,2,3,2,3,4,3] @?= 24
  , testCase "pickBestClump 9" $ pickBestClump [7,9,7,9,31,15,14] @?= 32
  , testCase "pickBestClump 10" $ pickBestClump [5,4,2,6,3,4,2,2,4,2,3,2,2,6,6,3,2,2,4,6,4,7,7,7] 
      @?= 50
  ]

directoryCountsTests :: TestTree
directoryCountsTests = testGroup "directoryCounts"
  [ testCase "directoryCounts 1" $ shouldReturnLogger (directoryCounts []) []
  , testCase "directoryCounts 2" $ shouldReturnLogger
      (directoryCounts ["/tmp/file.txt"]) [("tmp",1)]
  , testCase "directoryCounts 3" $ shouldReturnLogger
      (directoryCounts ["/tmp/output/file.txt"]) [("tmp",2),("output",1)]
  , testCase "directoryCounts 4" $ shouldReturnLogger
      (directoryCounts ["/tmp/output/file.txt","/tmp/output/file_2.txt","/tmp/case_1.yaml"])
      [("tmp",4),("output",2)]
  , testCase "directoryCounts 5" $ shouldReturnLogger (directoryCounts
      [ "/tmp/output/file.txt","/tmp/output/file_2.txt","/tmp/case_1.yaml"
      , "/home/compiled/script.py","/home/document.doc","/home/exercise.pdf"
      , "/home/Code.hs", "/home/Code.rs"
      ])
      [("home",6),("tmp",4),("output",2),("compiled",1)]
  , testCase "directoryCounts 6" $ shouldReturnLogger (directoryCounts
      [ "/home/tools/script.py","/home/code/Solve.hs","/home/code/Runner.hs"
      , "/tmp/output.yaml","/lib/compiled.pyc","/lib/hs/compiled.lhs"
      , "/bin/hlint","/bin/python3","/bin/vim"
      ])
      [("home",5),("bin",3),("lib",3),("code",2),("hs",1),("tmp",1),("tools",1)]
  ]

algaeGrowthTests :: TestTree
algaeGrowthTests = testGroup "algaeGrowth"
  [ testCase "algaeGrowth 1" $ algaeGrowth 1 1 [1] @?= 2
  , testCase "algaeGrowth 2" $ algaeGrowth 2 1 [1] @?= 4
  , testCase "algaeGrowth 3" $ algaeGrowth 3 1 [1] @?= 8
  , testCase "algaeGrowth 4" $ algaeGrowth 3 2 [1] @?= 4
  , testCase "algaeGrowth 5" $ algaeGrowth 4 2 [1] @?= 4
  , testCase "algaeGrowth 6" $ algaeGrowth 5 2 [1] @?= 8
  , testCase "algaeGrowth 7" $ algaeGrowth 1 5 [2,1,3,4,5,1,3,2] @?= 10
  , testCase "algaeGrowth 8" $ algaeGrowth 2 5 [2,1,3,4,5,1,3,2] @?= 12
  , testCase "algaeGrowth 9" $ algaeGrowth 10 6 [2,1,3,4,5,1,3,2] @?= 30
  , testCase "algaeGrowth 10" $ algaeGrowth 20 6 [2,1,3,4,5,1,3,2] @?= 96
  , testCase "algaeGrowth 11" $ algaeGrowth 50 6 [2,1,3,4,5,1,3,2] @?= 3072
  , testCase "algaeGrowth 12" $ algaeGrowth 100 7 [2,1,3,4,5,1,3,2] @?= 196608
  , testCase "algaeGrowth 13" $ algaeGrowth 350 7
      [2,1,3,4,5,1,3,2,6,4,1,3,1,5,4,2,3,6,7] @?= 21392098230009856
  ]
