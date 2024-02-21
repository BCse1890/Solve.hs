module M1Lecture7 where

import Data.Function
import qualified Data.List as L
import Data.Char 

data Student = Student
  { name :: String
  , mathScore :: Int
  , compositionScore :: Int
  , foreignLanguageScore :: Int
  , scienceScore :: Int
  , literatureScore :: Int
  }

-- students :: [Student]
-- students = [ 
--   Student "Jack" 10 10 10 10 10
--   , Student "Chris" 15 5 5 5 5]  

studentAwards :: [Student] -> (String, String, String)
studentAwards [] = ("", "", "")
studentAwards students = (nameMaths, nameSci, nameLit)
  where 
    nameMaths = name $ L.maximumBy (compare `on` mathScore) students
    nameSci   = name $ L.maximumBy (compare `on` scienceScore) students
    nameLit   = name $ L.maximumBy (compare `on` literatureScore) students



anyOverlap :: [(Int, Int)] -> Bool
anyOverlap input =  f firstSort
  where 
    firstSort = L.sortOn fst input 
    f :: [(Int, Int)] -> Bool
    f [] = False
    f [x] = False 
    f (a : b: rest) = (snd a >= fst b) || f (b:rest)
    

  


buildIntervals :: Int -> [Bool] -> [(Int, Int)]
buildIntervals startIndex values =  reverse . snd $ foldl f (startIndex, []) grouped
  where
    grouped = L.group values

    f (start, acc) sublist = 
      let l = length sublist in
      if (head sublist) then (start + l, (start, start + (l-1)) : acc)
      else (start + l, acc)
    
    

anagrams :: [String] -> [[String]]
anagrams inputs = L.sortOn (head) $ map (map snd) grouped
  where 
    letterTups = map (\ x -> (L.sort x, x)) inputs 
    grouped = L.groupBy (on (==) fst) (L.sort letterTups)

-- ["cab", "box", "ox", "abc", "xo", "oxb"]


buildMaze :: String -> Int -> [[((Int, Int), Bool)]]
buildMaze mazeString numRows =  L.groupBy (on (==) (fst.fst)) h
  where
    g = [ (rows, cols)   |  rows <- [0..(numRows - 1)], cols <- [0..topX] ]
    topX = (length mazeString) `div` numRows - 1
    boolMaze = map ('x' ==) mazeString
    h = zip g boolMaze


incrementingChunks :: [Int] -> [[Int]]
incrementingChunks [] = []
incrementingChunks numbers = reverse $ (map reverse) $ map (map snd) gr
  where
    gr = L.groupBy (on (==) fst) g
    (g, _, _) =  foldl f ((0, head numbers):[], head numbers, 0) (tail numbers)
    f (acc, prev, counter) current = 
      if current == prev + 1 then ((counter, current) :acc, current, counter) 
        else ((counter+1, current): acc, current, counter+1) 

-- [1, 2, 5, 2, 4, 5, 6, 7, 10, 11]
