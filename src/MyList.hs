{-# LANGUAGE NoImplicitPrelude #-}

module MyList where

import Prelude (Bool(..), error, Int, (-), (+), (*), (&&), (||), Ord(..), ($), Maybe(..), Eq(..), Ordering(..), Monad(..), (<$>), Show(..), not, undefined)

data MyList a = Nil | Cons a (MyList a)
  deriving (Show, Eq)

-- Module 1 Lecture 1
-- Pattern Matching
null' :: MyList a -> Bool
null' mylist = case mylist of
  Nil -> True
  _   -> False


head' :: MyList a -> a
head' Nil = error "no input provided"
head' (Cons a _ ) = a

tail' :: MyList a -> MyList a
tail' Nil = error "no input provided"
tail' (Cons _ (rest)) = rest

cons' :: a -> MyList a -> MyList a
cons' x Nil = Cons x Nil
cons' x mylist = Cons x mylist

-- Module 1 Lecture 2
-- Basic Recursion
atIndex :: MyList a -> Int -> a
atIndex Nil _        = error "You have entered an empty list"
atIndex (Cons x _) 0 = x
atIndex (Cons _ rest) n = atIndex rest (n-1)


last' :: MyList a -> a
last' Nil = error "Input is an empty list"
last' (Cons x Nil) = x
last' (Cons _ rest) = last' rest


find' :: (a -> Bool) -> MyList a -> Maybe a
find' _ Nil = Nothing
find' f (Cons x rest) = if (f x) then Just x else find' f rest
  

elem' :: (Eq a) => a -> MyList a -> Bool
elem' _ Nil          = False
elem' x (Cons y Nil) = if (x == y) then True else False
elem' x (Cons y rest) = if (x == y) then True else elem' x rest
   


and' :: MyList Bool -> Bool
and' Nil          = True
and' (Cons x Nil) = True && x
-- and' (Cons x rest) = True && x && (and' rest)
and' (Cons x rest) = x && (and' rest)

or' :: MyList Bool -> Bool
or' Nil = False
or' (Cons x Nil) = False || x
-- or' (Cons x rest) = False || x || (or' rest)
or' (Cons x rest) = x || (or' rest)


any' :: (a -> Bool) -> MyList a -> Bool
any' _ Nil = False
any' f (Cons x rest) = if (f x) then True else any' f rest

all' :: (a -> Bool) -> MyList a -> Bool
all' _ Nil   = True
all' f (Cons x rest) = if (f x) then all' f rest else False
   

isPrefixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isPrefixOf' Nil _   = True
isPrefixOf' _ Nil   = False
isPrefixOf' (Cons m restA) (Cons x restB) = if m == x then isPrefixOf' restA restB else False

isInfixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isInfixOf' Nil _ = True
isInfixOf' _ Nil = False
isInfixOf' (myList1) (Cons m myList2) =  case isPrefixOf' (myList1) (Cons m myList2) of
    True -> True
    False -> isInfixOf' myList1 myList2
   
    -- = if m == x then (isInfixOf' restA restB) else isInfixOf' (Cons m restA) restB
 


    -- Module 1 Lecture 3
-- Recusion with Accumulation
length' :: MyList a -> Int
length' Nil = 0
length' (Cons _ myList)  = 1 + length' myList

sum' :: MyList Int -> Int
sum' Nil = 0
sum' (Cons d myList) = d + sum' myList

product' :: MyList Int -> Int
product' Nil = 1
product' (Cons d ds) = d * product' ds

maximum' :: (Ord a) => MyList a -> a
maximum' Nil = error "Can't find max of empty mylist" 
maximum' (Cons d Nil) = d
maximum' (Cons d ds) = if (d >= maximum' ds) then d else (maximum' ds)

minimum' :: (Ord a) => MyList a -> a
minimum' Nil = error "Can't find max of empty mylist" 
minimum' (Cons d Nil) = d
minimum' (Cons d ds) = if (d <= minimum' ds) then d else (minimum' ds)

elemIndex' :: (Eq a) => a -> MyList a -> Maybe Int
elemIndex' _ Nil = Nothing
elemIndex' i (Cons c myList) = if (i == c) then (Just 0) 
  else (+1) <$> (elemIndex' i myList) 


-- Module 1 Lecture 4
-- Tail Recursion
sum'' :: MyList Int -> Int
sum'' = sum''Tail 0
  where
    sum''Tail :: Int -> MyList Int -> Int
    sum''Tail acc Nil = acc
    sum''Tail acc (Cons x xs) = sum''Tail (x + acc) xs


product'' :: MyList Int -> Int
product'' = prod''Tail 1
 where
    prod''Tail :: Int -> MyList Int -> Int
    prod''Tail acc Nil = acc
    prod''Tail acc (Cons x xs) = prod''Tail (x * acc) xs


and'' :: MyList Bool -> Bool
and'' = and''Tail True
  where
    and''Tail :: Bool -> MyList Bool -> Bool
    and''Tail acc Nil = acc
    and''Tail acc (Cons x xs) = and''Tail (x && acc) xs

    
or'' :: MyList Bool -> Bool
or'' = or''Tail False
  where
    or''Tail :: Bool -> MyList Bool -> Bool
    or''Tail acc Nil = acc
    or''Tail acc (Cons x xs) = or''Tail (x || acc) xs


any'' :: (a -> Bool) -> MyList a -> Bool
any'' = any''Tail False
  where
    any''Tail :: Bool -> (a -> Bool) -> MyList a -> Bool
    any''Tail acc _ Nil = acc
    any''Tail acc f (Cons x xs) = any''Tail (f x || acc) f xs


all'' :: (a -> Bool) -> MyList a -> Bool
all'' = all''Tail True
  where
    all''Tail :: Bool -> (a -> Bool) -> MyList a -> Bool
    all''Tail acc _ Nil = acc
    all''Tail acc f (Cons x xs) = all''Tail (f x && acc) f xs

maximum'' :: (Ord a) => MyList a -> a
maximum'' (Cons p ps) = all''Max p ps
  where
    all''Max :: (Ord a) => a -> MyList a -> a
    all''Max newMax Nil = newMax
    all''Max newMax (Cons x xs) 
      | x > newMax = all''Max x xs
      | x <= newMax  = all''Max newMax xs


minimum'' :: (Ord a) => MyList a -> a
minimum'' (Cons p ps) = all''Min p ps
  where
    all''Min :: (Ord a) => a -> MyList a -> a
    all''Min newMin Nil = newMin
    all''Min newMin (Cons x xs) 
      | x < newMin = all''Min x xs
      | x >= newMin  = all''Min newMin xs

elemIndex'' :: (Eq a) => a -> MyList a -> Maybe Int
elemIndex'' d = elemIndTail 0 d

elemIndTail :: (Eq a) => Int -> a -> MyList a -> Maybe Int
elemIndTail _ _ Nil = Nothing
elemIndTail index d (Cons p ps)
  | d == p = Just index
  | d /= p = elemIndTail (index+1) d ps
  -- | index == (length (p:ps)) = Nothing
  

-- Module 1 Lecture 5
-- List Accumulation
reverse' :: MyList a -> MyList a
reverse' = undefined

append' :: MyList a -> MyList a -> MyList a
append' = undefined

findIndices' :: (a -> Bool) -> MyList a -> MyList Int
findIndices' = undefined

isSuffixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isSuffixOf' = undefined

map' :: (a -> b) -> MyList a -> MyList b
map' = undefined

filter' :: (a -> Bool) -> MyList a -> MyList a
filter' = undefined

snoc' :: MyList a -> a -> MyList a
snoc' = undefined

init' :: MyList a -> MyList a
init' = undefined

concat' :: MyList (MyList a) -> MyList a
concat' = undefined

-- E.g. concatMap' (\a -> [a + 1, a + 2, a + 3]) [1, 5] = [2, 3, 4, 6, 7, 9]
concatMap' :: (a -> MyList b) -> MyList a -> MyList b
concatMap' = undefined

zip' :: MyList a -> MyList b -> MyList (a, b)
zip' = undefined

-- Module 1 Lecture 6
-- Folding
foldl'' :: (b -> a -> b) -> b -> MyList a -> b
foldl'' = undefined

foldr' :: (a -> b -> b) -> b -> MyList a -> b
foldr' = undefined

scanl' :: (b -> a -> b) -> b -> MyList a -> MyList b
scanl' = undefined

sum''' :: MyList Int -> Int
sum''' = undefined

map'' :: (a -> b) -> MyList a -> MyList b
map'' = undefined

-- Module 1 Lecture 7
-- Sorting, Grouping and HOF Patterns
maximumBy' :: (a -> a -> Ordering) -> MyList a -> a
maximumBy' = undefined

sortBy' :: (a -> a -> Ordering) -> MyList a -> MyList a
sortBy' = undefined

sort' :: (Ord a) => MyList a -> MyList a
sort' = undefined

sortOn' :: (Ord b) => (a -> b) -> MyList a -> MyList a
sortOn' = undefined

groupBy' :: (a -> a -> Bool) -> MyList a -> MyList (MyList a)
groupBy' = undefined

group' :: (Eq a) => MyList a -> MyList (MyList a)
group' = undefined

-- Module 1 Lecture 8
-- Set Functions
nub' :: (Eq a) => MyList a -> MyList a
nub' = undefined

delete' :: (Eq a) => a -> MyList a -> MyList a
delete' = undefined

intersect' :: (Eq a) => MyList a -> MyList a -> MyList a
intersect' = undefined

union' :: (Eq a) => MyList a -> MyList a -> MyList a
union' = undefined

-- Module 1 Lecture 9
-- Monadic Functions
mapM' :: (Monad m) => (a -> m b) -> MyList a -> m (MyList b)
mapM' = undefined

foldM' :: (Monad m) => (b -> a -> m b) -> b -> MyList a -> m b
foldM' = undefined

sequence' :: (Monad m) => MyList (m a) -> m (MyList a)
sequence' = undefined
