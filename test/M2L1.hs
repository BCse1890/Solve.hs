module Main where

import qualified Data.Set as St
import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified MySet as S
import M1Lecture10
import M2Lecture1
import Utils
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 1 Tests"
  [ setTests
  , practiceTests
  ]

setTests :: TestTree
setTests = testGroup "MySet Tests"
  [ nullTests
  , sizeTests
  , toListTests
  , memberTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ compartmentalizeTests
  , traverse2D4PTests
  , toggleLightsTests
  , collectPelletsTests
  , treeVisibilityTests
  , gameOfLifeTests
  ]

nullTests :: TestTree
nullTests = testGroup "null"
  [ testCase "null 1" $ S.null S.empty @?= True
  , testCase "null 2" $ S.null (S.singleton 1) @?= False
  , testCase "null 3" $ S.null (S.fromList [1, 2]) @?= False
  , testCase "null 4" $ S.null (S.insert "Hi" S.empty) @?= False
  , testCase "null 5" $ S.null (S.delete "Hi" (S.singleton "Hi")) @?= True
  ]

sizeTests :: TestTree
sizeTests = testGroup "size"
  [ testCase "size 1" $ S.size S.empty @?= 0
  , testCase "size 2" $ S.size (S.singleton 1) @?= 1
  , testCase "size 3" $ S.size (S.fromList [1, 2, 5, 5]) @?= 3
  , testCase "size 4" $ S.size (S.insert "Hi" S.empty) @?= 1
  , testCase "size 5" $ S.size (S.insert "Hi" (S.singleton "Hi")) @?= 1
  , testCase "size 6" $ S.size (S.delete "Hi" (S.singleton "Hi")) @?= 0
  ]

toListTests :: TestTree
toListTests = testGroup "toList"
  [ testCase "toList 1" $ S.toList S.empty @?= ([] :: [Int])
  , testCase "toList 2" $ S.toList (S.singleton 1) @?= [1]
  , testCase "toList 3" $ S.toList (S.fromList [1, 2, 5, 5]) @?= [1, 2, 5]
  , testCase "toList 4" $ S.toList (S.insert "Hi" (S.singleton "Hi")) @?= ["Hi"]
  , testCase "toList 5" $ S.toList (S.delete "Hi" (S.singleton "Hi")) @?= []
  , testCase "toList 6" $ S.toList
      (S.insert 10 (S.insert 2 (S.insert 4 (S.insert 7 (S.insert 8 (S.fromList [3, 9, 5, 1, 6]))))))
      @?= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  ]

memberTests :: TestTree
memberTests = testGroup "member"
  [ testCase "member 1" $ S.member 1 S.empty @?= False
  , testCase "member 2" $ S.member 1 (S.singleton 1) @?= True
  , testCase "member 3" $ S.member 2 (S.singleton 1) @?= False
  , testCase "member 4" $ S.member 2 (S.fromList [1, 2, 5, 5]) @?= True
  , testCase "member 5" $ S.member 5 (S.fromList [1, 2, 5, 5]) @?= True
  , testCase "member 6" $ S.member 6 (S.fromList [1, 2, 5, 5]) @?= False
  , testCase "member 7" $ S.member 5 (S.delete 5 (S.fromList [1, 2, 5, 5])) @?= False
  , testCase "member 8" $ S.member "Hi" (S.insert "Hi" (S.singleton "Hi")) @?= True
  , testCase "member 9" $ S.member "Hi" (S.delete "Hi" (S.singleton "Hi")) @?= False
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

traverse2D4PTests :: TestTree
traverse2D4PTests = testGroup "traverse2D4P"
  [ testCase "traverse2D4P 1" $ traverse2D4P (0,0) [] @?= 1
  , testCase "traverse2D4P 2" $ traverse2D4P (1,1) [Right,Left,Up,Down] @?= 5
  , testCase "traverse2D4P 3" $ traverse2D4P (1,1) [Right,Right,Right,Right] @?= 2
  , testCase "traverse2D4P 4" $ traverse2D4P (1,1) [Right,Right,Right,Right,Right] @?= 3
  , testCase "traverse2D4P 5" $ traverse2D4P (1,1) [Right,Left,Left,Right,Right] @?= 4
  , testCase "traverse2D4P 6" $ traverse2D4P (1,1) [Right,Left,Up,Down,Left,Right,Down,Up] @?= 5
  , testCase "traverse2D4P 7" $ traverse2D4P (1,1) [Right,Left,Up,Down,Right,Left,Up,Down,Right,Left,Up,Down,Right,Left,Up,Down] @?= 17
  , testCase "traverse2D4P 8" $ traverse2D4P (1,1)
      [Up,Right,Down,Left,Up,Left,Down,Right,Up,Up] @?= 8
  , testCase "traverse2D4P 9" $ traverse2D4P (1,1)
      [Up,Left,Right,Down,Up,Left,Right,Down,Left,Up,Down,Right,Left,Up,Down,Right] @?= 15
  ]

toggleLightsTests :: TestTree
toggleLightsTests = testGroup "toggleLights"
  [ testCase "toggleLights 1" $ toggleLights [] @?= 0
  , testCase "toggleLights 2" $ toggleLights [(On, 1, 10)] @?= 10
  , testCase "toggleLights 3" $ toggleLights [(Off, 1, 10)] @?= 0
  , testCase "toggleLights 4" $ toggleLights [(Toggle, 1, 10)] @?= 10
  , testCase "toggleLights 5" $ toggleLights [(On, 1, 10), (Off, 5, 8)] @?= 6
  , testCase "toggleLights 6" $ toggleLights [(On, 5, 9), (Off, 7, 8), (Toggle, 1, 12)] @?= 9
  , testCase "toggleLights 7" $ toggleLights
      [(On, 5, 9), (Off, 7, 8), (Toggle, 1, 12), (Toggle,1,25), (Off, 51, 55), (On, 53, 54)]
      @?= 18
  ]

collectPelletsTests :: TestTree
collectPelletsTests = testGroup "collectPellets"
  [ testCase "collectPellets 1" $ shouldReturnLogger (collectPellets [(0,0)] []) 0
  , testCase "collectPellets 2" $ shouldReturnLogger (collectPellets [(0,0), (0,3), (3,3), (3,0)]
      [(Right, 3), (Up, 3), (Left, 3), (Down, 3)]) 4
  , testCase "collectPellets 3" $ shouldReturnLogger (collectPellets [(1,1), (0,2), (2,3), (2,0)]
      [(Right, 3), (Up, 3), (Left, 3), (Down, 3)]) 3
  , testCase "collectPellets 4" $ shouldReturnLogger (collectPellets
      [(1,0),(2,0),(5,0),(3,3),(2,2),(0,1),(5,5),(0,3)]
      [(Right, 3), (Up, 3), (Left, 1), (Down, 1), (Left, 1), (Down, 2)]) 4
  , testCase "collectPellets 5" $ shouldReturnLogger (collectPellets [(2,0)]
      [(Right, 4), (Left, 4)]) 1
  , testCase "collectPellets 6" $ shouldReturnLogger (collectPellets [(-2,0)]
      [(Right, 4), (Left, 4)]) 0
  , testCase "collectPellets 7" $ shouldReturnLogger (collectPellets [(-10,-10),(10,10)]
      [(Down, 10), (Left, 10), (Right, 20), (Up, 20)]) 2
  ]

treeVisibilityTests :: TestTree
treeVisibilityTests = testGroup "treeVisibility"
  [ testCase "treeVisibility 1" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([(0,4),(0,5)], [], [])) 1
  , testCase "treeVisibility 2" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([(0,4),(0,6)], [], [])) 2
  , testCase "treeVisibility 3" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([(0,4),(0,5),(0,6)], [], [])) 1
  , testCase "treeVisibility 4" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([], [(4,0),(7,0)], [])) 2
  , testCase "treeVisibility 5" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([], [(4,0),(5,0)], [])) 1
  , testCase "treeVisibility 6" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([], [(4,0),(6,0)], [])) 1
  , testCase "treeVisibility 7" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([], [], [(4,4),(5,5)])) 1
  , testCase "treeVisibility 8" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([], [], [(4,4),(6,6)])) 1
  , testCase "treeVisibility 9" $ shouldReturnLogger (treeVisibility ((0,0),(0,1),(0,2)) ([], [], [(4,4),(7,7)])) 1
  , testCase "treeVisibility 10" $ shouldReturnLogger (treeVisibility ((0,0),(5,9),(0,2)) ([], [], [(4,4),(5,5)])) 2
  , testCase "treeVisibility 11" $ shouldReturnLogger (treeVisibility ((5,5),(6,5),(7,5)) ([], [(8,4)], [(3,7),(2,2)])) 3
  , testCase "treeVisibility 12" $ shouldReturnLogger (treeVisibility ((0,0), (3,9), (7,3))
      ( [(2,0),(3,0),(8,2),(7,4),(0,5),(7,5),(0,6),(0,7),(7,7),(0,8),(0,9),(1,9),(2,9),(4,9)]
      , [(2,1),(3,1),(2,3),(4,3),(9,5),(5,9)]
      , [(9,1), (1,3),(5,5),(6,9),(9,9)]
      )) 19
  ]

gameOfLifeTests :: TestTree
gameOfLifeTests = testGroup "gameOfLife"
  [ testCase "gameOfLife 1" $ shouldReturnLogger (gameOfLife (10, 10)
      (St.singleton (5,5))) St.empty
  , testCase "gameOfLife 2" $ shouldReturnLogger (gameOfLife (10, 10)
      (St.fromList [(5,5),(5,6)])) St.empty
  , testCase "gameOfLife 3" $ shouldReturnLogger (gameOfLife (10, 10)
      (St.fromList [(5,5),(5,6),(6,5),(6,6)]))
      (St.fromList [(5,5),(5,6),(6,5),(6,6)])
  , testCase "gameOfLife 4" $ shouldReturnLogger (gameOfLife (10, 10)
      (St.fromList [(0,0),(1,0),(2,0),(0,5),(0,6),(0,7),(9,7),(9,6),(9,5),(3,9),(4,9),(5,9)]))
      (St.fromList [(1,0),(1,1), (0,6),(1,6), (4,9),(4,8), (8,6),(9,6)])
  , testCase "gameOfLife 5" $ shouldReturnLogger (gameOfLife (10, 10)
      (St.fromList [(1,0),(1,1), (0,6),(1,6), (4,9),(4,8), (8,6),(9,6)])) St.empty
  , testCase "gameOfLife 6a" $ shouldReturnLogger (ev3 1) rotate3
  , testCase "gameOfLife 6b" $ shouldReturnLogger (ev3 2) orig3
  , testCase "gameOfLife 6c" $ shouldReturnLogger (ev3 3) rotate3
  , testCase "gameOfLife 6d" $ shouldReturnLogger (ev3 4) orig3
  , testCase "gameOfLife 7a" $ shouldReturnLogger (evG 1)
      (St.fromList [(2,8),(2,7),(1,7),(1,6),(0,8)])
  , testCase "gameOfLife 7b" $ shouldReturnLogger (evG 2)
      (St.fromList [(2,8),(2,7),(2,6),(1,6),(0,7)])
  , testCase "gameOfLife 7c" $ shouldReturnLogger (evG 3)
      (St.fromList [(3,7),(2,7),(2,6),(1,8),(1,6)])
  , testCase "gameOfLife 7d" $ shouldReturnLogger (evG 4)
      (St.fromList [(3,7),(3,6),(2,6),(2,8),(1,6)])
  , testCase "gameOfLife 7e" $ shouldReturnLogger (evG 8) (St.fromList (glide 2 <$> glider))
  , testCase "gameOfLife 7f" $ shouldReturnLogger (evG 12) (St.fromList (glide 3 <$> glider))
  , testCase "gameOfLife 7g" $ shouldReturnLogger (evG 16) (St.fromList (glide 4 <$> glider))
  ]
  where
    orig3 = St.fromList [(10,10),(10,11),(10,9)]
    rotate3 = St.fromList [(10,10),(9,10),(11,10)]
    ev3 = evolveStateM (gameOfLife (20,20)) orig3
    glider = [(1,9),(2,8),(0,7),(1,7),(2,7)]
    evG = evolveStateM (gameOfLife (10,10)) (St.fromList glider)
    glide n (x,y) = (x+n,y-n)
