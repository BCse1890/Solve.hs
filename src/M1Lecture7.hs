module M1Lecture7 where

import Data.Function
import qualified Data.List as L

data Student = Student
  { name :: String
  , mathScore :: Int
  , compositionScore :: Int
  , foreignLanguageScore :: Int
  , scienceScore :: Int
  , literatureScore :: Int
  }

studentAwards :: [Student] -> (String, String, String)
studentAwards = undefined

anyOverlap :: [(Int, Int)] -> Bool
anyOverlap = undefined

buildIntervals :: Int -> [Bool] -> [(Int, Int)]
buildIntervals startIndex values = undefined

anagrams :: [String] -> [[String]]
anagrams inputs = undefined

buildMaze :: String -> Int -> [[((Int, Int), Bool)]]
buildMaze mazeString numRows = undefined

incrementingChunks :: [Int] -> [[Int]]
incrementingChunks numbers = undefined
