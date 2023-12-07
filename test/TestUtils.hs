{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift)
import Control.Concurrent.Chan
import Data.Array
import Data.Char (chr)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty.HUnit

import MyList
import Utils hiding (Direction4(..))

mml :: [a] -> MyList a
mml = foldr Cons Nil

single :: MyList Int
single = Cons 1 Nil

double :: MyList Int
double = Cons 1 (Cons 2 Nil)

triple :: MyList Int
triple = mml [1, 2, 3]

many :: MyList String
many = mml ["Hello", "World", "!", "Goodbye"]

hmpMaze1 :: Array (Int, Int) Bool
hmpMaze1 = listArray ((0,0), (0,0))
  [ True ]

hmpMaze2 :: Array (Int, Int) Bool
hmpMaze2 = listArray ((0,0), (0,0))
  [ False ]

hmpMaze3 :: Array (Int, Int) Bool
hmpMaze3 = listArray ((0,0), (1,1))
  [ True, True, True, True ]

hmpMaze4 :: Array (Int, Int) Bool
hmpMaze4 = listArray ((0,0), (1,1))
  [ True, False, False, True ]

hmpMaze5 :: Array (Int, Int) Bool
hmpMaze5 = listArray ((0,0), (2,2))
  [ True, True, True
  , True, False, True
  , True, True, True
  ]

hmpMaze6 :: Array (Int, Int) Bool
hmpMaze6 = listArray ((0,0), (2,2))
  [ True, True, False
  , True, True, True
  , False, True, True
  ]

hmpMaze7 :: Array (Int, Int) Bool
hmpMaze7 = listArray ((0,0), (2,2))
  [ True, True, False
  , True, True, True
  , True, True, True
  ]

hmpMaze8 :: Array (Int, Int) Bool
hmpMaze8 = listArray ((0,0), (3,5))
  [ True, True, False, True, True, True
  , False, True, True, True, False, True
  , True, True, True, True, True, False
  , True, False, True, True, True, True
  ]

shouldReturnIO :: (Show a, Eq a) => IO a -> a -> Assertion
shouldReturnIO action expectedResult = do
  actualResult <- action
  actualResult @?= expectedResult

shouldReturnLogger :: (Show a, Eq a) => LoggingT IO a -> a -> Assertion
shouldReturnLogger action expectedResult = do
  actualResult <- runStdoutLoggingT action
  actualResult @?= expectedResult

testLogger :: (Show a, Eq a) => WriterLoggingT IO a -> a -> [String] -> Assertion
testLogger action expectedResult expectedLogs = do
  (actualResult, logs) <- runWriterLoggingT action
  actualResult @?= expectedResult
  (getStr <$> logs) @?= (toLogStr <$> expectedLogs)
  where
    getStr (_, _, _, s) = s

testLogger' :: (Show a, Eq a) =>
  StateT Int (MaybeT (WriterLoggingT IO)) a -> Maybe Int -> [String] -> Assertion
testLogger' action expectedResult expectedLogs = do
  (result, logs) <- runWriterLoggingT writerAction
  snd <$> result @?= expectedResult
  (getStr <$> logs) @?= (toLogStr <$> expectedLogs)
  where
    getStr (_, _, _, s) = s
    maybeAction = runStateT action 0
    writerAction = runMaybeT maybeAction

(@?~=) :: (Double, Double) -> (Double, Double) -> Assertion
actual@(a1, a2) @?~= expected@(e1, e2) = 
  let xs = abs (e1 - a1) < 0.000001
      ys = abs (e2 - a2) < 0.000001
  in  assertBool ("Expected close: " <> show actual <> " " <> show expected) (xs && ys)

listApproxEqual :: [Double] -> [Double] -> Assertion
listApproxEqual actual expected =
  let lengthEqual = length actual == length expected
      vsEqual = all (\(e, a) -> abs (e - a) < 0.000001) (zip actual expected)
  in  assertBool ("Expected close lists: " <> show actual <> " " <> show expected)
        (lengthEqual && vsEqual)

{- Evolve Logger -}

evolveStateM :: (Monad m) => (a -> m a) -> a -> Int -> m a
evolveStateM f initial x = if x <= 0
  then return initial
  else do
    next <- f initial
    evolveStateM f next (x - 1)

{- Huffman Trees -}

hf1 :: HuffmanTree
hf1 = HInternal (HInternal (HLeaf (chr 3) 1) (HLeaf 'b' 1) 2) (HLeaf 'a' 3) 5

hf2 :: HuffmanTree
hf2 = HInternal (HInternal (HLeaf (chr 3) 1) (HLeaf 't' 1) 2) (HInternal (HLeaf 'a' 1) (HLeaf 'c' 1) 2) 4

hf3 :: HuffmanTree
hf3 = HInternal
  (HInternal (HLeaf 'e' 4)
    (HInternal
      (HInternal (HLeaf (chr 3) 1) (HLeaf 'k' 1) 2)
      (HInternal (HLeaf 'l' 1) (HLeaf 't' 1) 2)
      4)
    8)
  (HInternal (HLeaf 'b' 5)
    (HInternal (HLeaf ' ' 3) (HLeaf 'a' 3) 6) 11)
  19

hf4 :: HuffmanTree
hf4 = HInternal
  (HLeaf 'f' 45)
  (HInternal
    (HInternal
      (HLeaf 'c' 12)
      (HLeaf 'd' 13)
      25)
    (HInternal
      (HInternal (HInternal (HLeaf (chr 3) 1) (HLeaf 'a' 4) 5) (HLeaf 'b' 9) 14)
      (HLeaf 'e' 16)
      30)
    55)
  100

{- Simple Graphs -}

emptyGraph :: Graph Int Int
emptyGraph = Graph HM.empty

g1Edges :: [(String, String, Int)]
g1Edges =
  [ ("A", "B", 1), ("A", "C", 20), ("A", "D", 100)
  , ("B", "E", 30), ("B", "D", 50), ("C", "D", 20)
  , ("E", "D", 1)
  ]

graph1 :: SimpleGraph
graph1 = Graph $ HM.fromList
  [ ("A", [("B", 1), ("C", 20), ("D", 100)])
  , ("B", [("E", 30), ("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  , ("E", [("D", 1)])
  ]

graph1u :: SimpleGraph
graph1u = Graph $ HM.fromList
  [ ("A", [("B", 1), ("C", 20), ("D", 100)])
  , ("B", [("A", 1), ("E", 30), ("D", 50)])
  , ("C", [("A", 20), ("D", 20)])
  , ("D", [("A",100),("B",50),("C",20),("E",1)])
  , ("E", [("B", 30), ("D", 1)])
  ]

g2Edges :: [(String, String, Int)]
g2Edges =
  [ ("A", "B", 10), ("A", "C", 20)
  , ("B", "E", 5), ("B", "D", 15)
  , ("C", "E", 1), ("D", "F", 12)
  , ("E", "D", 7), ("E", "F", 6)
  , ("F", "A", 3), ("F", "C", 1)
  , ("G", "A", 15)
  ]

graph2 :: SimpleGraph
graph2 = Graph $ HM.fromList
  [ ("A", [("B", 10), ("C", 20)])
  , ("B", [("E", 5), ("D", 15)])
  , ("C", [("E", 1)])
  , ("D", [("F", 12)])
  , ("E", [("D", 7), ("F", 6)])
  , ("F", [("A", 3), ("C", 1)])
  , ("G", [("A", 15)])
  ]

graph2u :: SimpleGraph
graph2u = Graph $ HM.fromList
  [ ("A", [("B", 10), ("C", 20),("F",3),("G",15)])
  , ("B", [("A", 10), ("E", 5), ("D", 15)])
  , ("C", [("A", 20), ("E", 1), ("F",1)])
  , ("D", [("B", 15), ("F", 12), ("E", 7)])
  , ("E", [("B", 5), ("C", 1), ("D", 7), ("F", 6)])
  , ("F", [("D",12), ("E",6), ("A", 3), ("C", 1)])
  , ("G", [("A", 15)])
  ]

g3Edges :: [(String, String, Int)]
g3Edges =
  [ ("1","2",5), ("1","3",15), ("1","4",10)
  , ("2","5",8), ("3","6",17), ("4","7",29)
  ]

g4Edges :: [(String, String, Int)]
g4Edges =
  [ ("1","2",5), ("1","3",15), ("1","4",10)
  , ("2","3",8), ("3","4",17), ("4","2",29)
  ]
