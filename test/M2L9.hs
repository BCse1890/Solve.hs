module Main where

import Data.ByteString (pack)
import Data.Word (Word8)
import qualified Data.List as L
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import M2Lecture9

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 9 Tests"
  [ memberTests
  , insertTests
  , deleteTests
  , leftShiftTests
  , rightShiftTests
  , lonelyNumberTests
  , decodeHuffmanTests
  , ballDropTests
  ]

memberTests :: TestTree
memberTests = testGroup "member"
  [ testCase "member 1" $ member 0 [1] @?= True
  , testCase "member 2" $ member 1 [1] @?= False
  , testCase "member 3" $ member 1 [2] @?= True
  , testCase "member 4" $ member 0 [2] @?= False
  , testCase "member 5" $ member 0 [14, 1] @?= True
  , testCase "member 6" $ member 1 [14, 1] @?= False
  , testCase "member 7" $ member 8 [14, 1] @?= False
  , testCase "member 8" $ member 10 [14, 1] @?= True
  , testCase "member 9" $ all (`member` [255,0,0]) [16..23] @?= True
  , testCase "member 10" $ any (`member` [255,0,0]) [0..15] @?= False
  , testCase "member 11" $ member 32 [14, 1] @?= False
  ]

insertTests :: TestTree
insertTests = testGroup "insert"
  [ testCase "insert 1" $ insert 1 [14,1] @?= [14,3]
  , testCase "insert 2" $ insert 8 [14,1] @?= [15,1]
  , testCase "insert 3" $ insert 25 [14,1] @?= [2,0,14,1]
  , testCase "insert 4" $ insert 7 [] @?= [128]
  , testCase "insert 5" $ insert 0 [] @?= [1]
  , testCase "insert 6" $ insert 8 [] @?= [1,0]
  , testCase "insert 7" $ insert 27 [2,0,14,1] @?= [10,0,14,1]
  , testCase "insert 8" $ insert 0 [14,1] @?= [14,1]
  , testCase "insert 9" $ insert 9 [14,1] @?= [14,1]
  , testCase "insert 10" $ insert 10 [14,1] @?= [14,1]
  , testCase "insert 11" $ insert 11 [14,1] @?= [14,1]
  ]

deleteTests :: TestTree
deleteTests = testGroup "delete"
  [ testCase "delete 1" $ delete 9 [14,1] @?= [12,1]
  , testCase "delete 2" $ delete 25 [2,0,14,1] @?= [14,1]
  , testCase "delete 3" $ delete 0 [1] @?= []
  , testCase "delete 4" $ delete 0 [2] @?= [2]
  , testCase "delete 5" $ delete 32 [2] @?= [2]
  , testCase "delete 6" $ delete 0 [14,1] @?= [14,0]
  , testCase "delete 7" $ delete 2 [14,1] @?= [14,1]
  , testCase "delete 8" $ delete 25 [10,0,14,1] @?= [8,0,14,1]
  , testCase "delete 9" $ delete 26 [10,0,14,1] @?= [10,0,14,1]
  ]

leftShiftTests :: TestTree
leftShiftTests = testGroup "leftShift"
  [ testCase "leftShift 1" $ leftShift [] @?= []
  , testCase "leftShift 2" $ leftShift [1] @?= [2]
  , testCase "leftShift 3" $ leftShift [10] @?= [20]
  , testCase "leftShift 4" $ leftShift [36,14] @?= [72,28]
  , testCase "leftShift 5" $ leftShift [10,1] @?= [20,2]
  , testCase "leftShift 6" $ leftShift [128] @?= [1,0]
  , testCase "leftShift 7" $ leftShift [10,255] @?= [21,254]
  , testCase "leftShift 8" $ leftShift [255,0,128] @?= [1,254,1,0]
  ]

rightShiftTests :: TestTree
rightShiftTests = testGroup "rightShift"
  [ testCase "rightShift 1" $ rightShift [] @?= []
  , testCase "rightShift 2" $ rightShift [1] @?= []
  , testCase "rightShift 3" $ rightShift [10] @?= [5]
  , testCase "rightShift 3" $ rightShift [5] @?= [2]
  , testCase "rightShift 4" $ rightShift [36,14] @?= [18,7]
  , testCase "rightShift 5" $ rightShift [10,1] @?= [5,0]
  , testCase "rightShift 6" $ rightShift [1,1] @?= [128]
  , testCase "rightShift 7" $ rightShift [10,255] @?= [5,127]
  , testCase "rightShift 8" $ rightShift [255,0,128] @?= [127,128,64]
  , testCase "rightShift 9" $ rightShift [1,7,0,1,14,3] @?= [131,128,0,135,1]
  ]

lonelyNumberTests :: TestTree
lonelyNumberTests = testGroup "lonelyNumber"
  [ testCase "lonelyNumber 1" $ lonelyNumber [1,1,2] @?= 2
  , testCase "lonelyNumber 2" $ lonelyNumber [0,1,0,2,1,3,2] @?= 3
  , testCase "lonelyNumber 3" $ lonelyNumber (([1..100] :: [Word8]) <> l1) @?= 4
  , testCase "lonelyNumber 4" $ lonelyNumber (([100,99..0] :: [Word8]) <> l2) @?= 55
  ]
  where
    l1 :: [Word8]
    l1 = L.delete (4 :: Word8) ([1..100] :: [Word8])
    l2 :: [Word8]
    l2 = L.delete (55 :: Word8) ([0..100] :: [Word8])

decodeHuffmanTests :: TestTree
decodeHuffmanTests = testGroup "decodeHuffman"
  [ testCase "decodeHuffman 1" $ shouldReturnLogger (decodeHuffman bs1 hf1) "aaba"
  , testCase "decodeHuffman 2" $ shouldReturnLogger (decodeHuffman bs2 hf1) "aaab"
  , testCase "decodeHuffman 3" $ shouldReturnLogger (decodeHuffman bs3 hf2) "cat"
  , testCase "decodeHuffman 4" $ shouldReturnLogger (decodeHuffman bs4 hf2) "act"
  , testCase "decodeHuffman 5" $ shouldReturnLogger (decodeHuffman bs5 hf3) "a bake beet babble"
  , testCase "decodeHuffman 6" $ shouldReturnLogger (decodeHuffman bs6 hf3) "a beet babble bake"
  , testCase "decodeHuffman 7" $ shouldReturnLogger (decodeHuffman bs7 hf4)
      "ffeefafeefeefafcefcefcfafddffdddffafffbbffbfffbfffccfdeedeeddfddeeddeefccfffbbbffffbbffccfcffccffff"
  ]
  where
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

ballDropTests :: TestTree
ballDropTests = testGroup "ballDrop"
  [ testCase "ballDrop 1" $ ballDrop (3,2) [1] [1] [1] [1] `shouldReturnLogger` True
  , testCase "ballDrop 2" $ ballDrop (3,2)
     (1 : replicate 62 0) (1 : replicate 62 0) (1 : replicate 0 62) (1 : replicate 0 62) 
     `shouldReturnLogger` False
  , testCase "ballDrop 3" $ ballDrop (5,4)
      (1 : replicate 49 0 <> (1 : replicate 12 0))
      (1 : replicate 21 0 <> (1 : replicate 40 0))
      (2 : replicate 60 0)
      (1 : replicate 23 0 <> (1 : replicate 37 0)) 
      `shouldReturnLogger` True
  , testCase "ballDrop 4" $ ballDrop (5,4)
      [51] [] [] []
      `shouldReturnLogger` False
  , testCase "ballDrop 5" $ ballDrop (5,4)
      [] [] [1] []
      `shouldReturnLogger` False
  , testCase "ballDrop 6" $ ballDrop (5,4)
      [] [23] [] []
      `shouldReturnLogger` False
  , testCase "ballDrop 7" $ ballDrop (5,4)
      [] [] [] [25]
      `shouldReturnLogger` False
  , testCase "ballDrop 8" $ ballDrop (1,1)
      [1,5,7] [16,3,30,2] [18,0,0,23,1,2] [25]
      `shouldReturnLogger` False
  , testCase "ballDrop 9" $ ballDrop (3,7)
      [1,5,7] [16,30] [18,23,1,2] [25]
      `shouldReturnLogger` True
  ]
