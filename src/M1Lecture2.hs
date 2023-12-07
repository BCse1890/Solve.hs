module M1Lecture2 where

import Crypto.Hash.MD5 (start, update, finalize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.List as L
import Data.Word (Word8, Word)
import Data.Maybe(fromJust)
import Data.List as DL


parallelCars :: (Word, Word) -> (Word, Word) -> Bool
parallelCars (x1, v1) (x2, v2) 
  | x1 == x2   = True
  | x2 > x1 && v2 > v1 = False 
  | x1 > x2 && v1 > v2 = False
  | otherwise = parallelCars (x1 + v1, v1) (x2+v2, v2) 
        
  
findKey :: String -> String -> Bool
findKey "" _ = True
findKey (x:xs) ys = x `elem` ys && findKey xs ys


firstProperFactor :: [Int] -> Maybe Int
firstProperFactor [] = Nothing
firstProperFactor [x] = Nothing
firstProperFactor (x:xs) 
  | (y /= x) && (y `mod` x == 0)  =  Just x
  | otherwise =  firstProperFactor xs
  where y = last (x:xs)


areAllFactors :: [Int] -> [Int] -> Bool
areAllFactors factors numbers = all (\ j -> agg `mod` (numbers !! j) == 0) factors
  where agg = sum (numbers)

hasIncrementingList :: [Int] -> [Int] -> Bool
hasIncrementingList starts numbers = any (\ j -> DL.isInfixOf [j, j+1, j+2] numbers) starts

hasIncrementingList' :: [Int] -> [Int] -> Bool
hasIncrementingList' starts (s: numbers') = case check_Increment (s:numbers') of
    Just r -> r `elem` starts
    Nothing -> hasIncrementingList' starts numbers'
  where
    check_Increment :: (Eq a, Num a) => [a] -> Maybe a
    check_Increment (p:q:r:_) 
      | (q == p+1) && (r == q+1) = Just p
      | otherwise = Nothing    

hashUntilIncrement :: ByteString -> Int -> Int
hashUntilIncrement key initial 
   | check_Increment = initial
   | otherwise = hashUntilIncrement key (initial+1)
  where
    h = mkHash key initial
    check_Increment = case first3 h of
      Nothing -> False
      Just (w, v, z) -> v == w+1 && z == v+1
  

first3 :: ByteString -> Maybe (Word8, Word8, Word8)
first3 bs = case take 3 (BS.unpack bs) of
  cs@[c1, c2, c3] -> if all (\d -> d >= 48 && d <= 57) cs
    then Just (c1 - 48, c2 - 48, c3 - 48)
    else Nothing
  _ -> Nothing

mkHash :: ByteString -> Int -> ByteString
mkHash key i = finalize c2
  where
    c1 = start key
    c2 = update c1 (pack . show $ i)
