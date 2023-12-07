module Main where

import Control.Monad.Logger
import Criterion
import Criterion.Main (defaultMain)

import qualified M2Lecture6 as M6
import qualified M2Lecture10 as M10

main :: IO ()
main = do
  defaultMain
    [ bgroup "Lists benchmark"
      [ bench "n=1000 q=10 k~=3000 Array" $ nf M6.intervalAddition l1
      , bench "n=1000 q=10 k~=3000 Sequence" $ nfIO (runNoLoggingT $ M10.intervalAddition l1)
      , bench "n=10000 q=20 k~=30000 Array" $ nf M6.intervalAddition l2
      , bench "n=10000 q=20 k~=30000 Sequence" $ nfIO (runNoLoggingT $ M10.intervalAddition l2)
      ]
    ]

l1 :: [((Int,Int),Int)]
l1 =
  [ ((1,301),16)
  , ((250,635),33)
  , ((700,902),18)
  , ((501,742),81)
  , ((13,152),103)
  , ((217,489),97)
  , ((841,975),54)
  , ((876,1000),35)
  , ((15,761),11)
  , ((362,658),44)
  ]

l2 :: [((Int,Int),Int)]
l2 =
  [ ((5872,7058), 69) -- ~1200
  , ((6369,8994), 32) -- ~2600
  , ((1936,3325), 12) -- ~1400
  , ((195, 519), 75)  -- ~325
  , ((6686,7796),81)  -- ~1100
  , ((9928,9973),22)  -- ~50
  , ((2255,4798),35)  -- ~2500
  , ((7347,8607),93)  -- ~1200
  , ((1147,2476),45)  -- ~1300
  , ((4113,6305),37)  -- ~2200
  , ((3128,6384),39)  -- ~3200
  , ((7187,9258),67)  -- ~2100
  , ((1,2057),20)     -- ~2050
  , ((4353,6131),29)  -- ~1800
  , ((3155,5377),17)  -- ~2200
  , ((1669,2246),96)  -- ~500
  , ((2511,4073),66)  -- ~1500
  , ((837,3311),68)   -- ~2500
  , ((1258,2243),44)  -- ~1000
  , ((3259,5906),85)  -- ~2500
  ]
