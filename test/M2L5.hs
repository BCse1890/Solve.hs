{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import M2Lecture5

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 5 Tests"
  [ binarySearchTests
  , addArraysTests
  , treeVisibilityReduxTests
  , functionPermutationTests
  , boggleSearchTests
  , bingoBoardTests
  ]

addArraysTests :: TestTree
addArraysTests = testGroup "addArrays"
  [ testCase "addArrays 1" $ shouldReturnLogger (addArrays
      [A.listArray (0,0) [1], A.listArray (0,0) [2]])
      (A.listArray (0,0) [3])
  , testCase "addArrays 2" $ shouldReturnLogger (addArrays
      [A.listArray (0,0) [1], A.listArray (1,1) [2]])
      (A.listArray (0,1) [1,2])
  , testCase "addArrays 3" $ shouldReturnLogger (addArrays
      [A.listArray (0,0) [1], A.listArray (3,3) [2]])
      (A.listArray (0,3) [1,0,0,2])
  , testCase "addArrays 4" $ shouldReturnLogger (addArrays
      [A.listArray (0,1) [1,2], A.listArray (1,2) [3,4]])
      (A.listArray (0,2) [1,5,4])
  , testCase "addArrays 5" $ shouldReturnLogger (addArrays
      [A.listArray (0,1) [1,2], A.listArray (4,5) [7,8]])
      (A.listArray (0,5) [1,2,0,0,7,8])
  , testCase "addArrays 6" $ shouldReturnLogger (addArrays
      [a1, A.listArray (2,3) [7,8]])
      (A.listArray (1,5) [5,10,15,1,10])
  , testCase "addArrays 7" $ shouldReturnLogger (addArrays [a1, a2])
      (A.listArray (1,10) [5,3,4,5,28,-5,2,-2,9,1])
  , testCase "addArrays 8" $ shouldReturnLogger (addArrays [a3, a2])
      (A.listArray (3,10) [3,16,20,5,-1,-1,17,3])
  , testCase "addArrays 9" $ shouldReturnLogger (addArrays [a3, a1, a2])
      (A.listArray (1,10) [5,3,10,17,30,5,-1,-1,17,3])
  , testCase "addArrays 9" $ shouldReturnLogger (addArrays
      [ A.listArray (1,10) [85,7,89,53,79,25,41,76,28,1]
      , A.listArray (6,15) [27,16,33,81,9,20,59,9,28,3]
      , A.listArray (11,20) [25,66,84,19,46,33,83,15,53,4]
      , A.listArray (16,25) [34,43,89,12,59,31,99,21,49,18]
      , A.listArray (21,30) [35,66,1,28,42,87,56,91,7,95]
      , A.listArray (26,35) [66,60,82,83,9,9,81,69,5,14]
      , A.listArray (31,40) [96,91,86,8,10,59,88,78,93,88]
      , A.listArray (36,45) [40,66,12,66,51,40,85,69,52,29]
      , A.listArray (41,50) [89,5,54,24,87,24,7,97,4,90]
      , A.listArray (46,55) [97,52,93,1,74,15,89,64,38,19]
      ])
      (A.listArray (1,55)
        [ 85,7,89,53,79
        , 52,57,109,109,10
        , 45,125,93,47,49
        , 67,126,104,65,63
        , 66,165,22,77,60
        , 153,116,173,90,104
        , 105,172,155,13,24
        , 99,154,90,159,139
        , 129,90,123,76,116
        , 121,59,190,5,164
        , 15,89,64,38,19
        ])
  ]
  where
    a1 = A.listArray (1,5) [5,3,7,1,10]
    a2 = A.listArray (3,10) [-3,4,18,-5,2,-2,9,1]
    a3 = A.listArray (3,10) [6,12,2,10,-3,1,8,2]

binarySearchTests :: TestTree
binarySearchTests = testGroup "binarySearch"
  [ testCase "binarySearch 1" $ map (binarySearch (flip (V.!)) (0,14) v1) [1..15] @?=
      Just <$> [0..14]
  , testCase "binarySearch 2" $ map (binarySearch (flip (V.!)) (0,14) v1) [(-10)..(-1)] @?=
      L.replicate 10 Nothing
  , testCase "binarySearch 3" $ binarySearch (flip (V.!)) (0,0) v0 1 @?= Just 0
  , testCase "binarySearch 4" $ binarySearch (flip (V.!)) (0,0) v0 2 @?= Nothing
  , testCase "binarySearch 5" $ binarySearch (flip (V.!)) (0,-1) v0' 0 @?= Nothing
  , testCase "binarySearch 6" $ map (binarySearch (flip (V.!)) (0,11) v2) [-5,-2,25,745,850,-10,14,28] @?=
      [Just 0, Just 1, Just 6, Just 11, Nothing, Nothing, Nothing, Nothing]
  , testCase "binarySearch 7" $ binarySearch (flip (V.!)) (0,2) v4 5 @?= Just 1
  , testCase "binarySearch 8" $ binarySearch (flip (V.!)) (0,2) v4 6 @?= Nothing
  , testCase "binarySearch 9" $ (binarySearch (flip (V.!)) (0,2) v5 3) `elem` [Just 2, Just 3] @?= True
  , testCase "binarySearch 10" $ binarySearch (flip (V.!)) (0,2) v5 6 @?= Nothing
  , testCase "binarySearch 11" $ binarySearch (flip (!!)) (0,14) l1 5 @?= Just 4
  , testCase "binarySearch 12" $ binarySearch (flip (A.!)) (0,14) a1 5 @?= Just 4
  , testCase "binarySearch 13" $ binarySearch (flip (A.!)) (1,15) a1' 5 @?= Just 5
  , testCase "binarySearch 14" $ binarySearch (flip (A.!)) (10,24) a1'' 5 @?= Just 14
  , testCase "binarySearch 15" $ map (binarySearchV v1) [1..15] @?=
      Just <$> [0..14]
  , testCase "binarySearch 16" $ map (binarySearchA a1') [1..15] @?=
      Just <$> [1..15]
  , testCase "binarySearch 17" $ binarySearch (flip S.index) (0,14) s1 5 @?= Just 4
  , testCase "binarySearch 18" $ binarySearchV v3 72 @?= Just 28
  , testCase "binarySearch 18" $ binarySearchV v3 8 @?= Just 2
  , testCase "binarySearch 18" $ binarySearchV v3 101 @?= Nothing
  , testCase "binarySearch 18" $ binarySearchV v3 (-2) @?= Nothing
  , testCase "binarySearch 18" $ binarySearchV v3 3 @?= Nothing
  ]
  where
    l0 = [1]
    l0' = []
    l1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
    l2 = [-5,-2,3,5,8,10,25,33,56,84,201,745]
    l3 = L.sort
      [ 50,26,75,65,76,1,52,61,90,56,14,61,95,53,44
      , 33,73,89,58,42,52,86,91,36,25,68,79,13,4,96
      , 31,50,84,72,100,24,98,90,76,57,83,87,99,39,8
      , 80,73,49,36,99
      ]
    l4 = [5,5,5]
    l5 = [1,2,3,3,3,5]
    [v0,v0',v1,v2,v3,v4,v5] = map V.fromList [l0,l0',l1,l2,l3,l4,l5]
    a1 = A.listArray (0,14) l1
    a1' = A.listArray (1,15) l1
    a1'' = A.listArray (10,24) l1
    s1 = S.fromList l1

treeVisibilityReduxTests :: TestTree
treeVisibilityReduxTests = testGroup "treeVisibilityRedux"
  [ testCase "treeVisibilityRedux 1" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((0,4),1),((0,5),1)])) 1
  , testCase "treeVisibilityRedux 2" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((0,4),1),((0,6),1)])) 2
  , testCase "treeVisibilityRedux 3" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((0,4),1),((0,5),1),((0,6),1)])) 1
  , testCase "treeVisibilityRedux 4" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,0),2),((7,0),2)])) 2
  , testCase "treeVisibilityRedux 5" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,0),8),((12,0),8)])) 1
  , testCase "treeVisibilityRedux 6" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,0),8),((13,0),8)])) 2
  , testCase "treeVisibilityRedux 7" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,0),7),((6,0), 4),((12,0),8)])) 2
  , testCase "treeVisibilityRedux 8" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,0),2),((5,0),2)])) 1
  , testCase "treeVisibilityRedux 9" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,0),2),((6,0),2)])) 1
  , testCase "treeVisibilityRedux 10" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,4),3),((5,5),3)])) 1
  , testCase "treeVisibilityRedux 11" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,4),3),((6,6),3)])) 1
  , testCase "treeVisibilityRedux 12" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(0,1),(0,2)) (mkGrid [((4,4),3),((7,7),3)])) 1
  , testCase "treeVisibilityRedux 13" $ shouldReturnLogger (treeVisibilityRedux
      ((0,0),(5,9),(0,2)) (mkGrid [((4,4),3),((5,5),3)])) 2
  , testCase "treeVisibilityRedux 14" $ shouldReturnLogger (treeVisibilityRedux
      ((5,5),(6,5),(7,5)) (mkGrid [((8,4),2),((3,7),3),((2,2),3)])) 3
  , testCase "treeVisibilityRedux 15" $ shouldReturnLogger
      (treeVisibilityRedux ((0,0), (3,9), (7,3))
        (mkGrid (trees1 <> trees2 <> trees3))) 19
  , testCase "treeVisibilityRedux 16" $ shouldReturnLogger
      (treeVisibilityRedux ((0,0), (3,9), (7,3))
        (mkGrid (trees1 <> trees2 <> trees4))) 17
  ]
  where
    mkGrid assocs = A.listArray ((0,0),(24,24)) (repeat 0) A.// assocs

    trees1 = (,1) <$>
      [(2,0),(3,0),(8,2),(7,4),(0,5),(7,5),(0,6),(0,7),(7,7),(0,8),(0,9),(1,9),(2,9),(4,9)]
    trees2 = (,2) <$> [(2,1),(3,1),(2,3),(4,3),(9,5),(5,9)]
    trees3 = (,3) <$> [(9,1), (1,3),(5,5),(6,9),(9,9)]
    trees4 = (,4) <$> [(9,1), (1,3),(5,5),(6,9),(9,9)]

functionPermutationTests :: TestTree
functionPermutationTests = testGroup "functionPermutation"
  [ testCase "functionPermutation 1" $ functionPermutation (V.fromList [2,5,0,1,3,4]) @?=
      V.fromList [0,4,2,5,1,3]
  , testCase "functionPermutation 2" $ functionPermutation (V.fromList [0,4,2,5,1,3]) @?=
      V.fromList [0,1,2,3,4,5]
  , testCase "functionPermutation 3" $ functionPermutation (V.fromList [2,1,0]) @?=
      V.fromList [0,1,2]
  , testCase "functionPermutation 4" $ functionPermutation (V.fromList [2,0,1]) @?=
      V.fromList [2,0,1]
  , testCase "functionPermutation 5" $ functionPermutation (V.fromList [0,1,2]) @?=
      V.fromList [0,1,2]
  , testCase "functionPermutation 6" $
      functionPermutation (V.fromList [1,5,8,3,2,10,11,6,7,9,4,0]) @?=
      V.fromList [6,11,10,3,5,0,8,2,4,9,1,7]
  ]

boggleSearchTests :: TestTree
boggleSearchTests = testGroup "boggleSearch"
  [ testCase "boggleSearch 1" $ shouldReturnLogger (boggleSearch grid1 "card") True
  , testCase "boggleSearch 2" $ shouldReturnLogger (boggleSearch grid1 "trim") True
  , testCase "boggleSearch 3" $ shouldReturnLogger (boggleSearch grid1 "ace") True
  , testCase "boggleSearch 4" $ shouldReturnLogger (boggleSearch grid1 "cake") True
  , testCase "boggleSearch 5" $ shouldReturnLogger (boggleSearch grid1 "ark") True
  , testCase "boggleSearch 6" $ shouldReturnLogger (boggleSearch grid1 "dome") True
  , testCase "boggleSearch 7" $ shouldReturnLogger (boggleSearch grid1 "meme") True
  , testCase "boggleSearch 8" $ shouldReturnLogger (boggleSearch grid1 "poem") True
  , testCase "boggleSearch 9" $ shouldReturnLogger (boggleSearch grid1 "gret") True
  , testCase "boggleSearch 10" $ shouldReturnLogger (boggleSearch grid1 "pope") False
  , testCase "boggleSearch 11" $ shouldReturnLogger (boggleSearch grid1 "xace") False
  , testCase "boggleSearch 12" $ shouldReturnLogger (boggleSearch grid1 "poke") False
  , testCase "boggleSearch 13" $ shouldReturnLogger (boggleSearch grid1 "poek") False
  , testCase "boggleSearch 14" $ shouldReturnLogger (boggleSearch grid1 "are") False
  ]
  where
    grid1 = A.listArray ((0,0), (3,3)) "cardekpogemetrim"

bingoBoardTests :: TestTree
bingoBoardTests = testGroup "bingoBoard"
  [ testCase "bingoBoard 1" $ shouldReturnLogger (bingoBoard [1] [1]) True
  , testCase "bingoBoard 2" $ shouldReturnLogger (bingoBoard [1] [2]) False
  , testCase "bingoBoard 3" $ shouldReturnLogger (bingoBoard [1,2,3,4] [2,5]) False
  , testCase "bingoBoard 4" $ shouldReturnLogger (bingoBoard [1,2,3,4] [2,1]) True
  , testCase "bingoBoard 5" $ shouldReturnLogger (bingoBoard [1,2,3,4] [1,3]) True
  , testCase "bingoBoard 6" $ shouldReturnLogger (bingoBoard [1,2,3,4] [2,4]) True
  , testCase "bingoBoard 7" $ shouldReturnLogger (bingoBoard g1 [60,55,78,66,52,43,5,18]) False
  , testCase "bingoBoard 8" $ shouldReturnLogger (bingoBoard g1 [55,9,92,76,3]) True
  , testCase "bingoBoard 9" $ shouldReturnLogger (bingoBoard g1 [65,76,61,32,6]) True
  , testCase "bingoBoard 10" $ shouldReturnLogger (bingoBoard g1 [82,9,77,32,89]) True
  , testCase "bingoBoard 11" $ shouldReturnLogger (bingoBoard g1 [60,9,61,98,30]) True
  , testCase "bingoBoard 12" $ shouldReturnLogger (bingoBoard g2 [60,9,61,98,30]) False
  , testCase "bingoBoard 13" $ shouldReturnLogger (bingoBoard g2 [60,9,61,98,30,50]) True
  , testCase "bingoBoard 14" $ shouldReturnLogger (bingoBoard g2 [55,82,9,77,91,89,32,60]) True
  ]
  where
    g1 = [ 60, 55, 63, 84,  6
         , 82,  9, 77 ,32, 89
         ,  4, 92, 61, 78,  2
         , 46, 76, 24, 98, 66
         , 65,  3, 25, 52, 30
         ]
    g2 = [ 60, 55, 63, 84,  6, 7
         , 82,  9, 77 ,32, 89, 91
         ,  4, 92, 61, 78,  2, 31
         , 46, 76, 24, 98, 66, 45
         , 65,  3, 25, 52, 30, 17
         ,  1,  8, 11, 12, 21, 50
         ]
