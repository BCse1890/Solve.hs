module Utils where

import Prelude hiding (Either(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

-- We treat these as X/Y coordinates, such that
-- moving "right" increases the first (x) coordinate
-- and moving "up" increases the second (y) coordinate.
type Coord2 = (Int, Int)
type Coord2f = (Double, Double)

data Direction4 = Up | Right | Down | Left
  deriving (Show, Eq)

data Movement4 = Movement4
  { direction :: Direction4
  , distance  :: Int
  }

data Direction8 =
  Up8 | UpRight | Right8 | DownRight |
  Down8 | DownLeft | Left8 | UpLeft
  deriving (Show, Eq)

moveD4 :: Int -> Coord2 -> Direction4 -> Coord2
moveD4 s (x, y) d4 = case d4 of
  Up    -> (x, y+s)
  Right -> (x+s, y)
  Down  -> (x, y-s)
  Left  -> (x-s, y)

moveD4' :: Movement4 -> Coord2 -> Coord2
moveD4' (Movement4 d4 s) (x, y) = case d4 of
  Up    -> (x, y+s)
  Right -> (x+s, y)
  Down  -> (x, y-s)
  Left  -> (x-s, y)

stepD4 :: Coord2 -> Direction4 -> Coord2
stepD4 (x, y) d4 = case d4 of 
   Up    -> (x, y+1)
   Right -> (x+1, y)
   Down  -> (x, y-1)
   Left  -> (x-1, y)
    

stepD4f :: Coord2f -> Direction4 -> Coord2f
stepD4f (x, y) d4 = case d4 of 
   Up    -> (x, y+ 1.0)
   Right -> (x+1.0, y)
   Down  -> (x, y-1.0)
   Left  -> (x-1.0, y)

moveD4f :: Double -> Coord2f -> Direction4 -> Coord2f
moveD4f s (x, y) d4 = case d4 of
  Up    -> (x, y+s)
  Right -> (x+s, y)
  Down  -> (x, y-s)
  Left  -> (x-s, y)

manhattanDistance :: Coord2 -> Coord2 -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

{- Turning -}
data Turn = TurnLeft | TurnRight
  deriving (Show, Eq, Ord, Enum)

turnDir :: Direction4 -> Turn -> Direction4
turnDir Up TurnLeft = Left
turnDir Up TurnRight = Right
turnDir Right TurnLeft = Up
turnDir Right TurnRight = Down
turnDir Down TurnLeft = Right
turnDir Down TurnRight = Left
turnDir Left TurnLeft = Down
turnDir Left TurnRight = Up

{- Neighbors Functions for 2D Coordinates -}

-- Unbounded
neighbors8U :: Coord2 -> [Coord2]
neighbors8U = neighbors8 (minBound, minBound) (maxBound, maxBound)

neighbors4U :: Coord2 -> [Coord2]
neighbors4U = neighbors4 (minBound, minBound) (maxBound, maxBound)

neighbors8 :: Coord2 -> Coord2 -> Coord2 -> [Coord2]
neighbors8 minB maxB (x, y) = filter (inBounds minB maxB)
  [ (x, y + 1), (x + 1, y + 1)
  , (x + 1, y), (x + 1, y - 1)
  , (x, y - 1), (x - 1, y - 1)
  , (x - 1, y), (x - 1, y + 1)
  ]

-- Inclusive Bounds
neighbors4 :: Coord2 -> Coord2 -> Coord2 -> [Coord2]
neighbors4 minB maxB (x, y) = filter (inBounds minB maxB)
  [ (x, y + 1)
  , (x + 1, y)
  , (x, y - 1)
  , (x - 1, y)
  ]

inBounds :: Coord2 -> Coord2 -> Coord2 -> Bool
inBounds (minX, minY) (maxX, maxY) (x', y') = x' >= minX && x' <= maxX && y' >= minY && y' <= maxY

-- Occurrence Maps

type OccMap a = OccMapI a Word
type OccMapBig a = OccMapI a Integer
type OccMapI a i = M.Map a i

emptyOcc :: OccMapI a i
emptyOcc = M.empty

addKey :: (Ord a, Integral i) => OccMapI a i -> a -> i -> OccMapI a i
addKey prevMap key count = undefined

incKey :: (Ord a, Integral i) => OccMapI a i -> a -> OccMapI a i
incKey prevMap key = undefined

subKey :: (Ord a, Integral i) => OccMapI a i -> a -> i -> OccMapI a i
subKey prevMap key count = undefined

decKey :: (Ord a, Integral i) => OccMapI a i -> a -> OccMapI a i
decKey prevMap key = undefined

-- Graph Basics

newtype Graph node cost = Graph
  { edgeMap :: HM.HashMap node [(node, cost)] }
  deriving (Show, Eq)

type SimpleGraph = Graph String Int
{- 
mkDirectedGraph :: (Hashable node) => [(node, node, cost)] -> Graph node cost
mkDirectedGraph edges = Graph $ foldr f HM.empty edges
  where
    f :: (Hashable node) =>
      (node, node, cost) -> HM.HashMap node [(node, cost)] -> HM.HashMap node [(node, cost)]
    f (n1, n2, c) prev =
      let firstPrev = fromMaybe [] (HM.lookup n1 prev)
          afterSecond = if not (HM.member n2 prev) then HM.insert n2 [] prev else prev
      in  HM.insert n1 ((n2, c) : firstPrev) afterSecond

mkUndirectedGraph :: (Hashable node) => [(node, node, cost)] -> Graph node cost
mkUndirectedGraph edges = Graph $ foldr f HM.empty edges
  where
    f :: (Hashable node) =>
      (node, node, cost) -> HM.HashMap node [(node, cost)] -> HM.HashMap node [(node, cost)]
    f (n1, n2, c) prev =
      let firstPrev = fromMaybe [] (HM.lookup n1 prev)
          secondPrev = fromMaybe [] (HM.lookup n2 prev)
          afterFirst = HM.insert n1 ((n2, c) : firstPrev) prev
      in  HM.insert n2 ((n1, c) : secondPrev) afterFirst

nodes :: (Hashable node) => Graph node cost -> [node]
nodes (Graph es) = HM.keys es

graphNeighborCosts :: (Hashable node) => Graph node cost -> node -> [(node, cost)]
graphNeighborCosts (Graph es) n1 = fromMaybe [] (HM.lookup n1 es)

graphNeighbors :: (Hashable node) => Graph node cost -> node -> [node]
graphNeighbors graph n1 = map fst (graphNeighborCosts graph n1)

graphCost :: (Hashable node) => Graph node cost -> node -> node -> Maybe cost
graphCost graph n1 n2 = snd <$> L.find ((==) n2 . fst) assocs
  where
    assocs = graphNeighborCosts graph n1 
-}

-- Huffman Tree
data HuffmanTree =
  HLeaf Char Int |
  HInternal HuffmanTree HuffmanTree Int
  deriving (Show)

huffmanVal :: HuffmanTree -> Int
huffmanVal (HLeaf _ v) = v
huffmanVal (HInternal _ _ v) = v 

instance Eq HuffmanTree where
  (HLeaf c1 v1) == (HLeaf c2 v2) = c1 == c2 && v1 == v2
  (HInternal l1 r1 v1) == (HInternal l2 r2 v2) = v1 == v2 && l1 == l2 && r1 == r2
  _ == _ = False

instance Ord HuffmanTree where
  compare (HLeaf c1 v1) (HLeaf c2 v2) = case compare v1 v2 of
    EQ -> compare c1 c2
    o -> o
  compare (HLeaf _ v1) (HInternal _ _ v2) = case compare v1 v2 of
    EQ -> LT
    o -> o
  compare (HInternal _ _ v1) (HLeaf _ v2) = case compare v1 v2 of
    EQ -> GT
    o -> o
  compare (HInternal l1 r1 v1) (HInternal l2 r2 v2) = case compare v1 v2 of
    EQ -> case compare l1 l2 of
      EQ -> compare r1 r2
      o' -> o'
    o -> o
