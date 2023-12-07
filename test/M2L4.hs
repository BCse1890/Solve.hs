{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import M2Lecture4
import Utils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 4 Tests"
  [ hiddenMessageTests
  , mazeDrillingTests
  , schoolyardRaceTests
  , powerStrikesTests
  , graphBasicsTests
  , edgePowerTests
  ]

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

mazeDrillingTests :: TestTree
mazeDrillingTests = testGroup "mazeDrilling"
  [ testCase "mazeDrilling 1" $ shouldReturnLogger (mazeDrilling (2,2)
      [] ((0,0), [Up,Right,Down]) ((0,1),[Right,Down,Left]))
      ((1,0),(0,0),(1,0))
  , testCase "mazeDrilling 2" $ shouldReturnLogger (mazeDrilling (2,2)
      [(0,1),(1,0)] ((0,0), [Up,Right,Down]) ((0,0),[Up,Right,Down]))
      ((0,0),(1,0),(1,0))
  , testCase "mazeDrilling 3" $ shouldReturnLogger (mazeDrilling (3,3)
      [(1,1)] ((0,2), [Right,Down,Left,Down,Right]) ((1,0),[Up,Right,Up]))
      ((0,1),(2,2),(1,0))
  , testCase "mazeDrilling 4" $ shouldReturnLogger (mazeDrilling (3,3)
      [(0,0),(2,2)] ((0,2), [Down,Down,Right,Up,Right,Up,Down]) ((1,1),[Up,Right,Down,Down,Left,Left]))
      ((1,1),(0,0),(2,1))
  , testCase "mazeDrilling 5" $ shouldReturnLogger (mazeDrilling (5,5)
      [(1,1),(2,1),(3,1),(3,2),(2,3)]
      ((4,2),[Left,Left,Down,Down,Left,Left,Up,Up,Up,Right,Right,Up])
      ((2,2),[Down,Down,Right,Right,Up,Up,Left,Up]))
      ((4,1),(3,3),(1,4))
  , testCase "mazeDrilling 6" $ shouldReturnLogger (mazeDrilling (10,10)
      [(1,0),(1,1),(2,1),(3,1),(4,1),(5,1),(5,2),(5,3),(5,4),(5,5),(4,5),(3,5),(2,5),(1,5),(5,0)]
      ((0,0),[Left,Up,Up,Right,Right,Down,Down,Right,Right,Up,Up,Right,Right,Up,Left,Left,Up,Left,Left,Up,Up])
      ((2,2),[Down,Down,Right,Right,Up,Up,Right,Left,Up,Up,Right,Right,Up,Up,Left,Left,Down]))
      ((0,6),(4,5),(4,6))
  ]

schoolyardRaceTests :: TestTree
schoolyardRaceTests = testGroup "schoolyardRace"
  [ testCase "schoolyardRace 1" $ shouldReturnLogger (schoolyardRace 5 [(0,1)] [(4,2)] []) (4,4,4,4)
  , testCase "schoolyardRace 2" $ shouldReturnLogger (schoolyardRace 5 [(0,1)] [(4,1)] []) (5,6,5,6)
  , testCase "schoolyardRace 3" $ shouldReturnLogger (schoolyardRace 6 [(0,1)] [(5,1)] []) (6,6,6,6)
  , testCase "schoolyardRace 4" $ shouldReturnLogger (schoolyardRace 5 [(0,1)] [(4,1)] [(2,1)]) (5,5,5,5)
  , testCase "schoolyardRace 5" $ shouldReturnLogger (schoolyardRace 5 [(0,1)] [(4,1)] [(2,1),(2,2)]) (5,6,5,6)
  , testCase "schoolyardRace 6" $ shouldReturnLogger (schoolyardRace 5 [(0,1),(1,1)] [(4,2)] []) (3,4,5,4)
  , testCase "schoolyardRace 7" $ shouldReturnLogger (schoolyardRace 5 [(1,1)] [(4,2),(3,2)] []) (3,3,3,5)
  , testCase "schoolyardRace 8" $ shouldReturnLogger (schoolyardRace 6 [(0,1)] [(5,0)] [(2,0)]) (5,8,5,8)
  , testCase "schoolyardRace 9" $ shouldReturnLogger (schoolyardRace 6 [(0,1)] [(5,0)] [(3,1)]) (7,7,7,7)
  , testCase "schoolyardRace 10" $ shouldReturnLogger (schoolyardRace 5 [(0,1),(0,2)] [(4,1),(4,2)] [(2,3)]) (5,5,6,8)
  , testCase "schoolyardRace 11" $ shouldReturnLogger (schoolyardRace 6 [(0,3),(0,1)] [(4,3),(5,0)] [(3,1)]) (6,5,8,6)
  , testCase "schoolyardRace 12" $ shouldReturnLogger (schoolyardRace 8
      [(0,1),(1,3),(1,4),(0,6),(2,8)] [(7,0),(7,3),(5,4),(6,6),(6,8)] [(4,1)]) (7,7,10,10)
  , testCase "schoolyardRace 13" $ shouldReturnLogger (schoolyardRace 10
     [(0,8),(1,6),(0,4),(1,3),(2,2)] [(9,5),(8,4),(9,2),(8,1),(7,0)]
     [(3,8),(7,6),(4,3),(3,0)]) (8,8,13,13)
  , testCase "schoolyardRace 14" $ shouldReturnLogger (schoolyardRace 5 [(0,1)] [(4,2)] [(4,1)]) (5,4,5,4)
  , testCase "schoolyardRace 15" $ shouldReturnLogger (schoolyardRace 25
     [(1,25),(2,25),(0,5),(0,2),(3,24),(1,24),(2,20),(0,10),(1,15),(0,8),(1,7),(3,3)]
     [(22,22),(22,15),(22,7),(22,16),(24,18),(22,23),(24,22),(22,20),(21,16),(24,17),(21,25),(23,5)]
     [(16,19),(11,20),(15,8),(10,6),(9,20),(8,24),(14,0),(13,8)]) (21,21,27,26)
  ]

powerStrikesTests :: TestTree
powerStrikesTests = testGroup "powerStrikes"
  [ testCase "powerStrikes 1" $ shouldReturnLogger (powerStrikes (10, 10)
      [ ((3,4),5,Right8)]) 0
  , testCase "powerStrikes 2" $ shouldReturnLogger (powerStrikes (10, 10)
      [ ((3,4),5,Right8),((3,3),9,UpRight)]) 1
  , testCase "powerStrikes 3" $ shouldReturnLogger (powerStrikes (10, 10)
      [ ((3,4),5,Right8),((4,8),9,Up8),((3,3),9,UpRight)]) 2
  , testCase "powerStrikes 4" $ shouldReturnLogger (powerStrikes (10, 10)
        (circleStrikes <> [((6,0),1,Right8)])) 8
  , testCase "powerStrikes 5" $ shouldReturnLogger (powerStrikes (10, 10)
        (circleStrikes <> [((6,0),1,Left8)])) 1
  , testCase "powerStrikes 6" $ shouldReturnLogger (powerStrikes (10, 10)
        (circleStrikes <> [((6,0),1,UpLeft)])) 8
  , testCase "powerStrikes 7" $ shouldReturnLogger (powerStrikes (10, 10)
      (circleStrikes <> [((6,0),1,UpLeft),((7,1),10,DownLeft)])) 9
  , testCase "powerStrikes 8" $ shouldReturnLogger (powerStrikes (10, 10)
      [ ((3,1),9,Left8), ((3,7),9,Left8),((6,1),9,Right8),((6,7),9,Right8)
      , ((9,4),6,Right8), ((9,1),3,Right8), ((9,7),3,Right8)
      ]) 0
  , testCase "powerStrikes 9" $ shouldReturnLogger (powerStrikes (10, 10)
      [ ((3,1),9,Left8), ((3,7),9,Left8),((6,1),9,Right8),((6,7),9,Right8)
      , ((9,4),7,Right8), ((9,1),3,Right8), ((9,7),3,Right8)
      , ((0,4),10,Right8)
      ]) 7
  , testCase "powerStrikes 10" $ shouldReturnLogger (powerStrikes (10, 10)
      [ ((3,1),9,Left8), ((3,7),9,Left8),((6,1),9,Right8),((6,7),9,Right8)
      , ((9,4),8,Right8), ((9,1),3,Right8), ((9,7),3,Right8)
      , ((0,4),10,Right8)
      ]) 7
  , testCase "powerStrikes 11" $ shouldReturnLogger (powerStrikes (25,25)
      [ ((12,12), 14, UpLeft), ((16,14), 7, Left8), ((8,9),8,Right8),((10,20),10,DownLeft)
      , ((5,24),4,UpLeft), ((0,13),24,Right8), ((4,23),13,Down8), ((12,7),20,Up8)
      , ((8,22),12,UpLeft),((16,11),10,UpLeft), ((2,20),11,UpLeft), ((24,19),12,Left8)
      , ((5,17),10,Left8), ((0,12),12,Left8), ((15,8),11,Right8), ((14,2),15,Down8)
      , ((18,19),1,Right8), ((23,15),4,Right8), ((9,7),4,Up8), ((4,16),9,Up8), ((18,21),6,Down8)
      , ((6,20),11,Up8), ((7,11),19,DownRight), ((2,23),20,UpLeft), ((0,17),21,Up8)
      , ((12,5),12,Up8), ((13,14),17,DownRight)
      ]) 1055
  ]
  where
    circleStrikes = 
      [ ((6,0),9,Down8), ((9,3),9,Right8), ((9,6),9,Right8), ((6,9), 9, Up8)
      , ((3,9),9,Up8), ((0,6),9,Left8), ((0,3),9,Left8), ((3,0),9,Down8)
      ]

graphBasicsTests :: TestTree
graphBasicsTests = testGroup "Graph Basics"
  [ testCase "mkDirectedGraph 1" $ mkDirectedGraph g1Edges @?= graph1
  , testCase "mkDirectedGraph 2" $ mkDirectedGraph g2Edges @?= graph2
  , testCase "mkDirectedGraph 3" $ mkDirectedGraph ([] :: [(Int, Int, Int)])
      @?= Graph (HM.empty)
  , testCase "mkDirectedGraph 4" $ mkDirectedGraph [("A","B",10)] @?=
      Graph (HM.fromList [("A", [("B",10)]), ("B",[])])
  , testCase "mkUndirectedGraph 1" $ mkUndirectedGraph g1Edges @?= graph1u
  , testCase "mkUndirectedGraph 2" $ mkUndirectedGraph g2Edges @?= graph2u
  , testCase "mkUndirectedGraph 3" $ mkUndirectedGraph ([] :: [(Int, Int, Int)])
      @?= emptyGraph
  , testCase "mkUndirectedGraph 4" $ mkUndirectedGraph [("A","B",10)] @?=
      Graph (HM.fromList [("A", [("B",10)]), ("B",[("A",10)])])
  , testCase "nodes 1" $ L.sort (nodes graph1) @?= ["A", "B", "C", "D", "E"]
  , testCase "nodes 2" $ L.sort (nodes graph2) @?= ["A", "B", "C", "D", "E", "F", "G"]
  , testCase "nodes 3" $ nodes emptyGraph @?= []
  , testCase "nodes 4" $ L.sort (nodes (mkDirectedGraph [("A","B",10)])) @?= ["A", "B"]
  , testCase "graphNeighborCosts 1" $ L.sort (graphNeighborCosts graph1 "A") @?=
      [("B", 1), ("C", 20), ("D", 100)]
  , testCase "graphNeighborCosts 2" $ L.sort (graphNeighborCosts graph1 "B") @?=
      [("D", 50), ("E", 30)]
  , testCase "graphNeighborCosts 3" $ L.sort (graphNeighborCosts graph1 "D") @?=
      []
  , testCase "graphNeighborCosts 4" $ L.sort (graphNeighborCosts graph2 "C") @?=
      [("E", 1)]
  , testCase "graphNeighborCosts 5" $ L.sort (graphNeighborCosts graph2 "E") @?=
      [("D", 7), ("F", 6)]
  , testCase "graphNeighbors 1" $ L.sort (graphNeighbors graph1 "A") @?=
      ["B", "C", "D"]
  , testCase "graphNeighbors 2" $ L.sort (graphNeighbors graph1 "B") @?=
      ["D", "E"]
  , testCase "graphNeighbors 3" $ L.sort (graphNeighbors graph1 "D") @?=
      []
  , testCase "graphNeighbors 4" $ L.sort (graphNeighbors graph2 "C") @?=
      ["E"]
  , testCase "graphNeighbors 5" $ L.sort (graphNeighbors graph2 "E") @?=
      ["D", "F"]
  , testCase "graphNeighbors 6" $ L.sort (graphNeighbors graph1u "B") @?=
      ["A","D", "E"]
  , testCase "graphCost 1" $ graphCost graph1 "A" "B" @?= Just 1
  , testCase "graphCost 2" $ graphCost graph1 "A" "D" @?= Just 100
  , testCase "graphCost 3" $ graphCost graph1 "A" "E" @?= Nothing
  , testCase "graphCost 4" $ graphCost graph1u "A" "E" @?= Nothing
  , testCase "graphCost 5" $ graphCost graph2 "D" "F" @?= Just 12
  , testCase "graphCost 6" $ graphCost graph2 "E" "D" @?= Just 7
  , testCase "graphCost 7" $ graphCost graph2 "B" "A" @?= Nothing
  , testCase "graphCost 8" $ graphCost graph2u "B" "A" @?= Just 10
  ]

edgePowerTests :: TestTree
edgePowerTests = testGroup "Graph Basics"
  [ testCase "edgePower 1" $ shouldReturnLogger (edgePower [] "A") 0
  , testCase "edgePower 2" $ shouldReturnLogger (edgePower [("A","B",5)] "A") 5
  , testCase "edgePower 3" $ shouldReturnLogger
      (edgePower [("A","B",5), ("B","C",7)] "A") 12
  , testCase "edgePower 4" $ shouldReturnLogger (edgePower
      [("A","B",5), ("A","D",4), ("B","C",7)] "A") 27
  , testCase "edgePower 5" $ shouldReturnLogger (edgePower
      [("A","B",5), ("A","C",4), ("B","C",7)] "A") 27
  , testCase "edgePower 6" $ shouldReturnLogger (edgePower
      [("A","B",5), ("A","D",4), ("B","C",7),("B","D",3)] "A") 30
  , testCase "edgePower 7" $ shouldReturnLogger (edgePower
      [("A","B",5), ("A","D",4), ("B","C",7),("B","D",3), ("C", "E",12), ("D", "E", 2)] "A") 44
  , testCase "edgePower 8" $ shouldReturnLogger (edgePower g1Edges "A") 2101
  , testCase "edgePower 9" $ shouldReturnLogger (edgePower g1Edges "D") 100051
  , testCase "edgePower 10" $ shouldReturnLogger (edgePower g2Edges "A") 9047
  , testCase "edgePower 11" $ shouldReturnLogger (edgePower g3Edges "1") 804
  , testCase "edgePower 12" $ shouldReturnLogger (edgePower g4Edges "1") 804
  ]
