module Main where

import Test.Tasty
import Test.Tasty.HUnit

import MyList
import M1Lecture4
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 1 Lecture 4 Tests"
  [ listTests
  , practiceTests
  ]

listTests :: TestTree
listTests = testGroup "MyList Tests"
  [ sumTests
  , productTests
  , andTests
  , orTests
  , allTests
  , anyTests
  , maximumTests
  , minimumTests
  , elemIndexTests
  ]

practiceTests :: TestTree
practiceTests = testGroup "Practice Problem Tests"
  [ buildNumberBetterTests
  , maxesAndMinsTests
  , elevationReduxTests
  , fireworksTests
  , math2dTests
  , tripletSumsTests
  ]

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "sum 1" $ sum'' Nil @?= 0
  , testCase "sum 2" $ sum'' single @?= 1
  , testCase "sum 3" $ sum'' double @?= 3
  , testCase "sum 4" $ sum'' triple @?= 6
  , testCase "sum 5" $ sum'' (mml [2, 6, 9, 15]) @?= 32
  ]

productTests :: TestTree
productTests = testGroup "product"
  [ testCase "product 1" $ product'' Nil @?= 1
  , testCase "product 2" $ product'' single @?= 1
  , testCase "product 3" $ product'' double @?= 2
  , testCase "product 4" $ product'' triple @?= 6
  , testCase "product 5" $ product'' (mml [2, 6, 9, 15]) @?= 1620
  ]

maximumTests :: TestTree
maximumTests = testGroup "maximum"
  [ testCase "maximum 1" $ maximum'' single @?= 1
  , testCase "maximum 2" $ maximum'' double @?= 2
  , testCase "maximum 3" $ maximum'' triple @?= 3
  , testCase "maximum 4" $ maximum'' (mml [3, 5, 2, 1, 0]) @?= 5
  , testCase "maximum 5" $ maximum'' many @?= "World"
  ]

minimumTests :: TestTree
minimumTests = testGroup "minimum"
  [ testCase "minimum 1" $ minimum'' single @?= 1
  , testCase "minimum 2" $ minimum'' double @?= 1
  , testCase "minimum 3" $ minimum'' triple @?= 1
  , testCase "minimum 4" $ minimum'' (mml [3, 5, 2, 1, 0]) @?= 0
  , testCase "minimum 5" $ minimum'' many @?= "!"
  ]

elemIndexTests :: TestTree
elemIndexTests = testGroup "elemIndex"
  [ testCase "elemIndex 1" $ elemIndex'' 1 single @?= Just 0
  , testCase "elemIndex 2" $ elemIndex'' 2 single @?= Nothing
  , testCase "elemIndex 3" $ elemIndex'' 2 triple @?= Just 1
  , testCase "elemIndex 3" $ elemIndex'' 3 triple @?= Just 2
  , testCase "elemIndex 4" $ elemIndex'' 1 (mml [3, 5, 2, 1, 0]) @?= Just 3
  , testCase "elemIndex 4" $ elemIndex'' 7 (mml [3, 5, 2, 1, 0]) @?= Nothing
  , testCase "elemIndex 5" $ elemIndex'' "!" many @?= Just 2
  , testCase "elemIndex 5" $ elemIndex'' "Word" many @?= Nothing
  ]

andTests :: TestTree
andTests = testGroup "and"
  [ testCase "and 1" $ and'' Nil @?= True
  , testCase "and 2" $ and'' (Cons True Nil) @?= True
  , testCase "and 3" $ and'' (Cons True (Cons False Nil)) @?= False
  ]

orTests :: TestTree
orTests = testGroup "or"
  [ testCase "or 1" $ or'' Nil @?= False
  , testCase "or 2" $ or'' (Cons True Nil) @?= True
  , testCase "or 3" $ or'' (Cons True (Cons False Nil)) @?= True
  , testCase "or 3" $ or'' (Cons False (Cons False Nil)) @?= False
  ]

allTests :: TestTree
allTests = testGroup "all"
  [ testCase "all 1" $ all'' even Nil @?= True
  , testCase "all 2" $ all'' even triple @?= False
  , testCase "all 3" $ all'' (\s -> length s < 10) many @?= True
  , testCase "all 4" $ all'' (\s -> length s > 2) many @?= False
  ]

anyTests :: TestTree
anyTests = testGroup "any"
  [ testCase "any 1" $ any'' even Nil @?= False
  , testCase "any 2" $ any'' even triple @?= True
  , testCase "any 3" $ any'' (\s -> length s < 10) many @?= True
  , testCase "any 4" $ any'' (\s -> length s > 2) many @?= True
  , testCase "any 5" $ any'' null many @?= False
  ]

buildNumberBetterTests :: TestTree
buildNumberBetterTests = testGroup "buildNumberBetter"
  [ testCase "buildNumberBetter 1" $ buildNumberBetter [] @?= 0
  , testCase "buildNumberBetter 2" $ buildNumberBetter [0] @?= 0
  , testCase "buildNumberBetter 3" $ buildNumberBetter [9] @?= 9
  , testCase "buildNumberBetter 4" $ buildNumberBetter [2, 3] @?= 23
  , testCase "buildNumberBetter 5" $ buildNumberBetter [2, 4, 0, 3] @?= 2403
  , testCase "buildNumberBetter 6" $ buildNumberBetter [8, 5, 7, 0, 3, 2, 1, 9] @?= 85703219
  ]

maxesAndMinsTests :: TestTree
maxesAndMinsTests = testGroup "maxesAndMins"
  [ testCase "maxesAndMins 1" $ maxesAndMins [] @?= (0, 0)
  , testCase "maxesAndMins 2" $ maxesAndMins [5] @?= (0, 0)
  , testCase "maxesAndMins 3" $ maxesAndMins [1, 5, 5] @?= (1, 0)
  , testCase "maxesAndMins 4" $ maxesAndMins [1, 0, -1, -2] @?= (0, 3)
  , testCase "maxesAndMins 5" $ maxesAndMins [1, 5, 5, 3, 2, 0, 6, -1, -1, 10] @?= (3, 2)
  ]

elevationReduxTests :: TestTree
elevationReduxTests = testGroup "elevationRedux"
  [ testCase "elevationRedux 1" $ elevationRedux "" @?= Nothing
  , testCase "elevationRedux 2" $ elevationRedux "duddudd" @?= (Just 1)
  , testCase "elevationRedux 3" $ elevationRedux
      "uuuuuuuuuuddddddddddddddduuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuudddddddddduuduuduuduuduud"
      @?= (Just 21)
  , testCase "elevationRedux 4" $ elevationRedux "uuuuuddddduduu" @?= Nothing
  ]

fireworksTests :: TestTree
fireworksTests = testGroup "fireworks"
  [ testCase "fireworks 1" $ fireworks (5, 10) 2 14 [] [] @?= 0
  , testCase "fireworks 2" $ fireworks (5, 10) 2 14 [9, -3, 8, 1, -1, 4, 3] [] @?= 3
  , testCase "fireworks 3" $ fireworks (5, 10) 2 14 [] [-5, 1, 2] @?= 1
  , testCase "fireworks 4" $ fireworks (5, 10) 2 14 [9, -3, 8, 1, -1, 4, 3] [-5, 1, 2] @?= 4
  , testCase "fireworks 5" $ fireworks (3, 5) 2 14 [9, -3, 8, 1, -1, 4, 3] [-5, 1, 2] @?= 2
  , testCase "fireworks 6" $ fireworks (3, 5) (-2) 6 [9, -3, 8, 1, -1, 4, 3] [-5, 1, 2] @?= 0
  ]

math2dTests :: TestTree
math2dTests = testGroup "math2d"
  [ testCase "math2d 1" $ math2d [] @?= 0
  , testCase "math2d 2" $ math2d [[1, 2, 3]] @?= 5
  , testCase "math2d 3" $ math2d [[1, 2, 3, 4]] @?= 20
  , testCase "math2d 4" $ math2d [[1, 2, 3, 4, 5]] @?= 25
  , testCase "math2d 5" $ math2d [[1, -2, 3, 4, -5]] @?= -1
  , testCase "math2d 6" $ math2d [[1, 2, 3, 4],[5, 6, 7, 8]] @?= 1256
  , testCase "math2d 7" $ math2d [[1, 2, 3, 4, 5],[5, 6, 7, 8, 9]] @?= 1505
  , testCase "math2d 8" $ math2d [[1, 2, 3], [-4, 5, 3], [2, 2, 2]] @?= 22
  , testCase "math2d 9" $ math2d [[4,3,-2], [6,2,13]] @?= 45
  ]

tripletSumsTests :: TestTree
tripletSumsTests = testGroup "tripletSums"
  [ testCase "tripletSums 1" $ tripletSums [] @?= 0
  , testCase "tripletSums 2" $ tripletSums [1,2,3] @?= 0
  , testCase "tripletSums 3" $ tripletSums [7,4,3] @?= 2
  , testCase "tripletSums 4" $ tripletSums [7,4,3,6] @?= 8
  , testCase "tripletSums 5" $ tripletSums [7,4,3,6,4] @?= 12
  , testCase "tripletSums 6" $ tripletSums [7,4,3,6,4,4] @?= 4
  , testCase "tripletSums 7" $ tripletSums [6, 7, 4, 2, 35, 17, 3, 4] @?= 11
  ]
