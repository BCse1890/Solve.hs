module M1Lecture10 where

import Data.Function (on)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.List as L

import Utils

evolveState :: (a -> a) -> a -> Int -> a
evolveState = undefined

evolveUntil :: (a -> a) -> a -> (a -> Bool) -> a
evolveUntil = undefined

runFSM ::
  (Enum a) =>
  (Char -> a -> a) ->
  (a -> Bool) ->
  a ->
  String ->
  Bool
runFSM = undefined

runPDA ::
  (Enum a) =>
  (Char -> (a, [Char]) -> (a, [Char])) ->
  (a -> Bool) ->
  a ->
  String ->
  Bool
runPDA = undefined

fishPopulation :: Int -> Int -> Int
fishPopulation numYears fishEaten = undefined

treeGrowth :: Int -> Int -> Int
treeGrowth = undefined

countCars :: (Int, Int) -> ([Int], [Int], [Int], [Int]) -> Int -> Int
countCars timers (north, south, east, west) time = undefined

playPacman :: (Coord2, Coord2, Coord2) -> Bool
playPacman (blinky, inky, pinky) = undefined

data RegexState =
  RegexFailure
  deriving (Eq, Enum)

solveRegex :: String -> Bool
solveRegex = undefined

data CFGState =
  CFGInitial |
  CFGSuccess |
  CFGFailure 
  deriving (Show, Eq, Enum)

solveCFG :: String -> Bool
solveCFG = undefined
