module GraphUtils where

import Data.Array
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)

type AdjacencyGraph = M.Map String [(String, Int)]

adjacencyGraphUnweightedNeighbors :: AdjacencyGraph -> String -> [String]
adjacencyGraphUnweightedNeighbors graph current = case M.lookup current graph of
  Nothing -> []
  Just edges -> fst <$> edges

adjacencyGraphNeighbors :: AdjacencyGraph -> String -> [(String, Int)]
adjacencyGraphNeighbors graph current = fromMaybe [] (M.lookup current graph)

graph1 :: AdjacencyGraph
graph1 = M.fromList
  [ ("A", [("B", 1), ("C", 20), ("D", 100)])
  , ("B", [("E", 30), ("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  , ("E", [("D", 1)])
  ]

graph2 :: AdjacencyGraph
graph2 = M.fromList
  [ ("A", [("B", 10), ("C", 20)])
  , ("B", [("E", 5), ("D", 15)])
  , ("C", [("E", 1)])
  , ("D", [("F", 12)])
  , ("E", [("D", 7), ("F", 6)])
  , ("F", [("A", 3), ("C", 1)])
  , ("G", [("A", 15)])
  ]

type Graph2D a = Array (Int, Int) a

data Cell = Empty | Wall | Goal
  deriving (Show, Eq)

neighbors4Wall :: Graph2D Cell -> (Int, Int) -> [(Int, Int)]
neighbors4Wall graph c = filter ((/= Wall) . (graph !)) (neighbors4' graph c)

neighbors4Dist :: Graph2D Int -> (Int, Int) -> [((Int, Int), Int)]
neighbors4Dist graph c = map (\c' -> (c', graph ! c')) (neighbors4' graph c)

neighbors4' :: Graph2D a -> (Int, Int) -> [(Int, Int)]
neighbors4' arr (r, c) = catMaybes [maybeUp, maybeRight, maybeDown, maybeLeft]
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds arr
    maybeUp = if r <= minRow then Nothing else Just (r - 1, c)
    maybeRight = if c >= maxCol then Nothing else Just (r, c + 1)
    maybeDown = if r >= maxRow then Nothing else Just (r + 1, c)
    maybeLeft = if c <= minCol then Nothing else Just (r, c - 1)

graph2D1 :: Graph2D Cell
graph2D1 = listArray ((0, 0), (4, 4))
  [ Empty, Goal, Empty, Empty, Goal
  , Empty, Wall, Empty, Empty, Empty
  , Empty, Goal, Empty, Wall, Goal
  , Goal, Empty, Empty, Empty, Wall
  , Empty, Goal, Empty, Empty, Goal
  ]

graph2D2 :: Graph2D Cell
graph2D2 = listArray ((0, 0), (4, 4))
  [ Empty, Empty, Empty, Empty, Empty
  , Empty, Wall, Empty, Empty, Wall
  , Empty, Empty, Empty, Wall, Goal
  , Empty, Empty, Empty, Empty, Wall
  , Empty, Empty, Empty, Empty, Empty
  ]

graph2D3 :: Graph2D Int
graph2D3 = listArray ((0, 0), (4, 4))
  [ 3, 0, 8, 2, 0
  , 4, 5, 2, 1, 4
  , 9, 0, 6, 9, 0
  , 0, 1, 7, 3, 5
  , 2, 0, 3, 6, 0
  ]

graph2D4 :: Graph2D Int
graph2D4 = listArray ((0, 0), (4, 4))
  [ 3, 2, 8, 2, 9
  , 4, 5, 2, 1, 4
  , 9, 9, 6, 9, 9
  , 8, 1, 7, 3, 5
  , 2, 9, 3, 6, 3
  ]

aStarGraph :: Graph2D Cell
aStarGraph = ogGraph // (goal : wallChanges1 ++ wallChanges2)
  where
    ogGraph = listArray ((0,0), (999, 999)) (replicate 1000000 Empty)
    goal = ((500, 500), Goal)
    wallIndices1 = filter (/= 450) [1..499]
    wallIndices2 = [500..950]
    wallChanges1 = concatMap (\i -> [((499,i), Wall), ((i,499), Wall)]) wallIndices1
    wallChanges2 = concatMap (\i -> [((1,i), Wall), ((i,1), Wall)]) wallIndices2

aStarGraph2 :: Graph2D Cell
aStarGraph2 = ogGraph // (goal : wallChanges1 ++ wallChanges2)
  where
    ogGraph = listArray ((0,0), (999, 999)) (replicate 1000000 Empty)
    goal = ((500, 500), Goal)
    wallIndices1 = [1..499]
    wallIndices2 = [500..999]
    wallChanges1 = concatMap (\i -> [((499,i), Wall), ((i,499), Wall)]) wallIndices1
    wallChanges2 = concatMap (\i -> [((1,i), Wall), ((i,1), Wall)]) wallIndices2
