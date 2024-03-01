module M1Lecture8 where

import Data.Function
import qualified Data.List as L
import qualified Data.List.Split as S
import Prelude hiding (Either(..))
import Data.Char
import Utils 



commonToken :: [String] -> String
commonToken = commonT ""
  where
    commonT acc [] = reverse acc
    commonT acc (x:y:z:rest) = 
      let s = L.intersect z (L.intersect x y) 
      in case s of
        [a] -> commonT (a : acc) rest
        _   -> error "Multiple common tokens."
    commonT  acc _ = error "Input not in blocks of 3."
    
    
inp = ["input", "reject", "tornado", "realize", "criminal", "isotope", "left", "egregious", "rife"] :: [String]


{- 
compartmentalize :: String -> Int -> Maybe Char

 Your input is a single string, as well as a number. It is guaranteed that the length of the string is
 divisible by the number. Your first job is to split the word into the number of parts given by the
 number; these are your "compartments". Then you must find the letter that is common to each
 compartment.
 
 So if your inputs are "ibegpgdeniemquem" and 4, you should return Just 'e'. The four
 compartments are 'ibeg', 'pgde', 'niem' and 'quem', and 'e' is the only common letter
 here. If there is no common letter or multiple common letters, you should return Nothing
-}

compartmentalize :: String -> Int -> Maybe Char
compartmentalize input numBuckets = case common of
    [a] -> Just a
    _   -> Nothing

  where
    bucketlen = length input `div` numBuckets
    chunked = S.chunksOf bucketlen input
    common = foldl f (head chunked) (tail chunked)
    f acc bucket = L.intersect bucket acc

input = "ibegpgdeniemquem"
        
{- 
traverse2D :: Coord2 -> [Direction4] -> Int

fold over direction list, using Direction4 from Utils.hs. stepD4 takes prev coord and 
direction as parameters to given end coord. Cons into acc and enter end coord into tuple as 
new prev coord. Use L.nub on reversed acc to remove duplicates, length give number of coords
in list.
-}


traverse2D :: Coord2 -> [Direction4] -> Int
traverse2D start dirs = length $ L.nub coords
  where
    coords = reverse $ fst $ foldl f ([start], start) dirs
    f (acc, prev) dir = 
      let newCoord = stepD4 prev dir
      in (newCoord : acc, newCoord)

dirs = [Up,Right,Down,Left,Up,Left,Down,Right,Up,Up] :: [Direction4]


{- 
smallestMultiples :: [Int] -> [Int] -> [Int]

Fold over the modulo list. Use f function to check if each modulo is a factor of 
searchlist and filter searchList to keep those modulos. If list contains one or more values,
select minimum, cons this into acc and delete this from searchList. Select fst from tuple
and reverse consed acc list to give result. 

-}

smallestMultiples :: [Int] -> [Int] -> [Int]
smallestMultiples search modulos = res
  where
    res = reverse . fst $ foldl f ([], search) modulos
    f (acc, searchList) m =  case filter (\ x -> x `mod` m == 0 ) searchList of 
      [] -> (acc, searchList)
      modList -> ((minimum modList) : acc, L.delete (minimum modList) searchList)


searchList = [16, 9, 12, 34, 26, 18] :: [Int]
modulo = [4, 17, 3, 5, 6] :: [Int]


{- 
grabbingLetters :: [String] -> (Int, Int, Int) -> String

recursePick takes the string identified by index from bag. Then deletes that string from bag
by using L.splitAt at index, to give tuple, taking tail of snd in tuple to delete the head. 
Then (++) together to give the end bag. Each endbag is starting bag for next run of fn.
 (++) the three collections together, nub to remove duplicated letters and sort to provide 
 result.

-}

grabbingLetters :: [String] -> (Int, Int, Int) -> String
grabbingLetters bag (i1, i2, i3) = result 
  where
    result = L.sort . L.nub $ fst first ++ fst second ++ fst third
    first = recursePick bag i1
    second = recursePick (snd first) i2
    third = recursePick (snd second) i3
    
    recursePick startBag i = 
      let collection = startBag !! (i-1)
          bag' = L.splitAt (i - 1) startBag
          endBag = (fst bag') ++ (tail $ snd bag')
      in (collection, endBag)



trial =  ["ab", "ca", "bc", "ehis", "zafg"] 
trial2 = ["a", "z", "f", "c", "b", "y", "x"]
{- test5 = ["z", "y", "x", "a", "b", "c", "d", "m", "o"] (9,8,7)
test6 = ["z", "y", "x", "a", "b", "c", "d", "m", "o"] (7,8,7)
test7 = ["z", "y", "x", "a", "b", "c", "d", "m", "o"] (5,5,5) -}


{- 

grabbingMoreLetters :: [String] -> String

L.permutations gives full set of permutations for bag, take first 3 so all 3 element perms 
included. Fold over these sublists, (++) the 3 component string elements together to a single
string. L.maximumBy (compare `on` length) str will pick max length string, but doesn't provide
tie breaker requiring alphabetical sorting. maxLength gives length of longest string. filter 
out all strings shorter than maxLength. Sort the resultant list alphabetically. Head of this
list gives the first lexographically. 

-}

grabbingMoreLetters :: [String] -> String
grabbingMoreLetters bag = maxes
  where
    perms =  map (take 3) (L.permutations bag)
    str = map (L.nub . L.sort) $ foldl f [] perms
    f acc [xs, ys, zs] = (xs ++ ys ++ zs):acc
    maxLength = length $ L.maximumBy (compare `on` length) str
    maxes = head . L.sort $ filter (\x -> (length x == maxLength)) str
    

