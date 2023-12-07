{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Ae
import Data.ByteString (pack)
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import M2Lecture10
import Utils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 10 Tests"
  [ upgradedAssemblyTests
  , pick2ClumpsTests
  , budgetCalculationTests
  , encodeHuffmanTests
  , skylineAreaTests
  , intervalAdditionTests
  ]

upgradedAssemblyTests :: TestTree
upgradedAssemblyTests = testGroup "upgradedAssembly"
  [ testCase "upgradedAssembly 1" $ shouldReturnLogger
      (upgradedAssembly [LoadValue "Eax" 4, PrintR "Eax"]) [4]
  , testCase "upgradedAssembly 2" $ shouldReturnLogger
      (upgradedAssembly [LoadValue "a" 4, LoadValue "b" (-3), AddR "a" "b", PrintR "a"])
      [1]
  , testCase "upgradedAssembly 3" $ shouldReturnLogger
      (upgradedAssembly
        [ LoadValue "a" 4
        , LoadValue "b" 2
        , LoadValue "c" 7
        , AddR "b" "c"
        , SubR "a" "b"
        , PrintR "a"
        , PrintR "b"
        , SubR "a" "a"
        , AddR "a" "c"
        , MultR "b" "a"
        , PrintR "b"
        , PrintR "c"
        , LoadValue "a" 3
        , SubR "c" "a"
        , PrintR "c"
        ]
      )
      [-5, 9, 63, 7, 4]
  , testCase "upgradedAssembly 4" $ shouldReturnLogger
      (upgradedAssembly
        [ LoadValue "x" 4
        , LoadValue "y" 11
        , LoadValue "z" (-40)
        , AndR "x" "y"
        , PrintR "x"
        , LoadValue "x" 4
        , OrR "x" "y"
        , PrintR "x"
        , LShiftR "x" 3
        , PrintR "x"
        , RShiftR "y" 2
        , PrintR "y"
        , RShiftR "z" 2
        , PrintR "z"
        , XorR "x" "y"
        , PrintR "x"
        , XorR "z" "x"
        , PrintR "z"
        ]
      )
      [0,15,120,2,-10,122,-116]
  ]

pick2ClumpsTests :: TestTree
pick2ClumpsTests = testGroup "pick2Clumps"
  [ testCase "pick2Clumps 1" $ shouldReturnLogger (pick2Clumps []) 0
  , testCase "pick2Clumps 2" $ shouldReturnLogger (pick2Clumps [1]) 2
  , testCase "pick2Clumps 3" $ shouldReturnLogger (pick2Clumps [1,1,1]) 6
  , testCase "pick2Clumps 4" $ shouldReturnLogger (pick2Clumps [1,2]) 4
  , testCase "pick2Clumps 5" $ shouldReturnLogger (pick2Clumps [1,1,2]) 6
  , testCase "pick2Clumps 6" $ shouldReturnLogger (pick2Clumps [2,3,4]) 10
  , testCase "pick2Clumps 7" $ shouldReturnLogger (pick2Clumps [2,3,3,4,4]) 19
  , testCase "pick2Clumps 8" $ shouldReturnLogger (pick2Clumps [5,6,8,8,2,3,2,3,2,3,4,3]) 45
  , testCase "pick2Clumps 9" $ shouldReturnLogger (pick2Clumps [7,9,7,9,30,14,13]) 63
  , testCase "pick2Clumps 10" $ shouldReturnLogger (pick2Clumps
      [5,4,2,6,3,4,2,2,4,2,3,2,2,6,6,3,2,2,4,6,4,7,7,7]) 97
  , testCase "pick2Clumps 11" $ shouldReturnLogger (pick2Clumps
      [2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]) 90
  , testCase "pick2Clumps 12" $ shouldReturnLogger (pick2Clumps
      [125,612,611,130,127,56,55,55,57,488,131,130,130, 64,65,66,66,64,17,18,35]) 1748
  ]

budgetCalculationTests :: TestTree
budgetCalculationTests = testGroup "budgetCalculation"
  [ testCase "budgetCalculation 1" $ shouldReturnLogger (budgetCalculation (f "1.57")) 1.57
  , testCase "budgetCalculation 2" $ shouldReturnLogger
      (budgetCalculation (f "[1.57,4.35,16.30]")) 22.22
  , testCase "budgetCalculation 3" $ shouldReturnLogger (budgetCalculation
      (f "{\"one\": 1.57, \"two\": 4.35,\"three\":16.30}")) 22.22
  , testCase "budgetCalculation 4" $ shouldReturnLogger (budgetCalculation
      (f "{\"one\": 1.57, \"two\": [4.35,16.30]}")) 22.22
  , testCase "budgetCalculation 5" $ shouldReturnLogger (budgetCalculation
      (f "{\"one\": 1.57, \"two\": [4.35,16.30,{\"five\":22.22}]}")) 44.44
  , testCase "budgetCalculation 6" $ shouldReturnLogger (budgetCalculation
      (f "{\"shoes\": 87.90,\"paint\":36.25,\"tools\":[\"hammer\",14.33,\"screws\",9.82]}")) 148.3
  , testCase "budgetCalculation 7" $ shouldReturnLogger (budgetCalculation
      (f "[1.57,\"credit\",0.90]")) 0.67
  , testCase "budgetCalculation 8" $ shouldReturnLogger (budgetCalculation
      (f "{\"one\":1.57,\"credit\":0.90}")) 0.67
  , testCase "budgetCalculation 9" $ shouldReturnLogger (budgetCalculation
      (f "{\"shoes\": 87.90,\"paint\":36.25,\"tools\":[\"hammer\",14.33,\"credit\",\"screws\",9.82],\"credit\":{\"nails\":5.89,\"other\":{\"candy\":1.42}}}")) 121.35
  , testCase "budgetCalculation 10" $ shouldReturnLogger (budgetCalculation
      (f "{\"expenses\":100.0,\"credit\":[5.0,6.0,\"credit\",3.0]}")) 86.0
  ]
  where
    f = fromJust . Ae.decode

encodeHuffmanTests :: TestTree
encodeHuffmanTests = testGroup "encodeHuffman"
  [ testCase "encodeHuffman 1" $ shouldReturnLogger (encodeHuffman hf1 "aaba") bs1
  , testCase "encodeHuffman 2" $ shouldReturnLogger (encodeHuffman hf1 "aaab") bs2
  , testCase "encodeHuffman 3" $ shouldReturnLogger (encodeHuffman hf2 "cat") bs3
  , testCase "encodeHuffman 4" $ shouldReturnLogger (encodeHuffman hf2 "act") bs4
  , testCase "encodeHuffman 5" $ shouldReturnLogger (encodeHuffman hf3 "a bake beet babble") bs5
  , testCase "encodeHuffman 6" $ shouldReturnLogger (encodeHuffman hf3 "a beet babble bake") bs6
  , testCase "encodeHuffman 7" $ shouldReturnLogger (encodeHuffman hf4 s7) bs7
  ]
  where
    s7 = "ffeefafeefeefafcefcefcfafddffdddffafffbbffbfffbfffccfdeedeeddfddeeddeefccfffbbbffffbbffccfcffccffff"
    bs1 = pack [216]
    bs2 = pack [232]
    bs3 = pack [228]
    bs4 = pack [180]
    bs5 = pack
      [ 250 -- 11111010
      , 234 -- 11101010
      , 104 -- 01101000
      , 31  -- 00011111
      , 94  -- 01011110
      , 152 -- 10011000
      , 64  -- 01000000
      ]
    bs6 = pack
      [ 250 -- 11111010
      , 7   -- 00000111
      , 215 -- 11010111
      , 166 -- 10100110
      , 53  -- 00110101
      , 212 -- 11010100
      , 64  -- 01000000
      ]
    bs7 = pack
      [ 63  -- 00111111 ffee
      , 101 -- 01100101 faf
      , 251 -- 11111011 eef
      , 246 -- 11110110 eef
      , 83  -- 01010011 afc
      , 167 -- 10100111 efce
      , 70  -- 01000110 fcf
      , 86  -- 01010110 afd
      , 150 -- 10010110 dffd
      , 211 -- 11010011 ddff
      , 35  -- 00100011 afff
      , 116 -- 01110100 bbff
      , 209 -- 11010001 bfff
      , 162 -- 10100010 bfff
      , 69  -- 01000101 ccfd
      , 254 -- 11111110 ee
      , 255 -- 11111111 dee
      , 106 -- 01101010 ddf
      , 223 -- 11011111 dde
      , 237 -- 11101101 edd
      , 253 -- 11111101 eef
      , 32  -- 00100000 ccfff
      , 221 -- 11011101 bb
      , 208 -- 11010000 bffff
      , 221 -- 11011101 bb
      , 36  -- 00100100 ffcc
      , 66  -- 01000010 fcff
      , 64  -- 01000000 ccffff
      , 192 -- 11000000
      ]

skylineAreaTests :: TestTree
skylineAreaTests = testGroup "skylineArea"
  [ testCase "skylineArea 1" $ shouldReturnLogger (skylineArea []) 0
  , testCase "skylineArea 2" $ shouldReturnLogger (skylineArea [(5, 9, 10)]) 40
  , testCase "skylineArea 3" $ shouldReturnLogger (skylineArea [(1,4,3), (7,9,5)]) 19
  , testCase "skylineArea 4" $ shouldReturnLogger (skylineArea [(1,9,3), (3,6,8)]) 39
  , testCase "skylineArea 5" $ shouldReturnLogger (skylineArea [(0,10,10), (1,9,3), (3,6,8)]) 100
  , testCase "skylineArea 6" $ shouldReturnLogger
      (skylineArea [(5,7,13), (1,9,3), (3,6,8),(0,18,1)]) 64
  , testCase "skylineArea 7" $ shouldReturnLogger (skylineArea [(0,1000000,2)]) 2000000
  , testCase "skylineArea 8" $ shouldReturnLogger
      (skylineArea [(0,2000000,2),(500000,1500000,3)]) 5000000
  , testCase "skylineArea 9" $ shouldReturnLogger (skylineArea [(0,10,2),(1000000,1000010,4)]) 60
  ]

intervalAdditionTests :: TestTree
intervalAdditionTests = testGroup "intervalAddition"
  [ testCase "intervalAddition 1" $ shouldReturnLogger (intervalAddition [((1,3),4)]) 4
  , testCase "intervalAddition 2" $ shouldReturnLogger (intervalAddition [((1,1),5)]) 5
  , testCase "intervalAddition 3" $ shouldReturnLogger (intervalAddition [((1,1),5),((2,2),6)]) 6
  , testCase "intervalAddition 4" $ shouldReturnLogger (intervalAddition [((1,4),5),((2,6),6)] )11
  , testCase "intervalAddition 5" $ shouldReturnLogger (intervalAddition
      [((1,100),1),((2,10),2),((90,99),3),((10,90),4)]) 8
  , testCase "intervalAddition 6" $ shouldReturnLogger (intervalAddition
      [((1,3),6),((3,5),4),((6,10),7),((4,7),15)]) 22
  , testCase "intervalAddition 7" $ shouldReturnLogger (intervalAddition
      [ ((56, 74), 13)
      , ((19, 98), 34)
      , ((21, 52), 19)
      , ((47, 90),  2)
      , ((21, 85), 21)
      , ((66, 69), 97)
      , ((15, 83), 70)
      , ((19, 98), 68)
      , (( 3, 50), 88)
      , ((43, 99), 94)
      , ((13, 83), 72)
      , ((22, 75), 45)
      ]) 516
  ]
