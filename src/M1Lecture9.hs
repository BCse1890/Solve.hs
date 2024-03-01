{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module M1Lecture9 where

import Control.Monad (foldM)
import Control.Monad.Logger
import Control.Monad.State
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.Text (pack)
import Prelude hiding (Either(..))
import Utils


commonToken :: (MonadFail m) => [String] -> m String
commonToken = commonT ""
  where
    commonT acc [] = return (reverse acc)
    commonT acc (x:y:z:rest) = 
      let s = L.intersect z (L.intersect x y) 
      in case s of
        [a] -> commonT (a : acc) rest
        _   -> fail "Multiple common tokens."
    commonT  _ _ = fail "Input not in blocks of 3."

    
compartmentalize :: (MonadFail m) => String -> Int -> m Char
compartmentalize input numBuckets = case common of
    [a] -> return a
    []  -> fail "No common letters found."
    _ -> fail "Multiple common letters found."

  where
    bucketlen = length input `div` numBuckets
    chunked = S.chunksOf bucketlen input
    common = foldl f (head chunked) (tail chunked)
    f acc bucket = L.intersect bucket acc

-- input = "ibegpgdeniemquem"


anyOverlap :: (MonadLogger m) => [(Int, Int)] -> m Bool
anyOverlap input =  f firstSort
  where 
    firstSort = L.sortOn fst input 
    f (a : b: rest) = do
        if snd a >= fst b
          then logDebugN (pack $ show a <> " and " <> show b <> " overlap!" ) >> return True
          else logDebugN (pack $ show a <> " and " <> show b <> " do not overlap!" ) >> f (b:rest) 
    f _ = logDebugN (pack  "No overlap found!") >> return False

-- check = runStdoutLoggingT $ anyOverlap [(4, 10), (1, 6)]


traverse2D :: (MonadLogger m) => Coord2 -> [Direction4] -> m Int
traverse2D start dirs = fmap ( length . L.nub ) coords
  where
    coords = reverse . fst <$> foldM f ([start], start) dirs
    f (acc, prev) dir = 
      let newCoord = stepD4 prev dir
      in  logDebugN (pack $ "Visiting: " <> show newCoord)  >> return (newCoord : acc, newCoord)


-- dirs = [Up,Right,Down,Left,Up,Left,Down,Right,Up,Up] :: [Direction4]


data ListQueue a = ListQueue
  { inputList :: [a]
  , outputList :: [a]
  }

  {- 
Queue Implementation
 We've restricted ourselves to using lists throughout this whole module, and Haskell's list type
 acts best as a "stack" data structure- you can only efficiently add or remove items from one end
 (that is, "Last-In-First-Out" or LIFO).

 A Queue is a list-like data structure that makes it efficient to add items to one end and then
 remove them from the other ("First-In-First-Out" or FIFO). In Module 2, we'll learn about the
 Sequence, which is Haskell's preferred queue data structure.

 But even just with what you know about lists, you can implement a very basic queue! In the
 module, you'll see the data type ListQueue, which has two list fields. You can modify the field
 names, but do not add any more fields to the type.

 Your job is to implement the enqueue and dequeue functions, which both use the State
 monad since they change the underlying structure. You should write them so that the amortized
 complexity of each operation is O(1). This means that a single operation might actually be
 O(n) (where n is the existing size of the queue), but any series of n operations is still O(n).
  -}

enqueue :: (MonadState (ListQueue a) m) => a -> m ()
enqueue x = do
  (ListQueue lIn lOut) <- get
  put (ListQueue (x:lIn) lOut)

  
{- dequeue :: (MonadState (ListQueue a) m) => m (Maybe a) 
First test if outQueue is populated. If so, pop from head of Qlist. If not, reverse inQueue, pop from head of Qlist
and populate this reversed list into outQ (so providing a FIFO data structure).
-}

dequeue :: (MonadState (ListQueue a) m) => m (Maybe a)
dequeue = do
  (ListQueue qIn qOut) <- get
  case qOut of
    (x:xs) -> put (ListQueue qIn xs) >> return (Just x)
    [] -> case reverse qIn of
      [] -> return Nothing
      (y:ys) -> put (ListQueue [] ys) >> return (Just y)

{- 
Mixing Monads

 Re-implement math2d from lecture 6, but using 3(!) different monads. First, you should use
 MonadFail and return an error if the rows do not all have the same size. Second, whenever
 you complete the processing for a row, you should log a message like so, where the first row
 shows up with index 1:

 After row 1, value is 10

 Finally, the whole function should operate by holding the accumulated value within the State
 monad. This means you should source the original value from the monadic context instead of
 beginning with 0. You also need not return the value explicitly. Consumers should pull the value
 out of the monad as well
-}

{- 
Use foldM to iterate over rows using f_vert. Need to add an index for rowNo, using this
 as acc. Use LogDebugN to log values, returning the rowNo. Discard this in final result 
 by return (). Iterate over colums for each row using f_horiz. Keep ongoing acc value
 from original programme in State, checking for whether even or odd row and modifying 
 State by +y or *y. Return the Bool for evenCol as the acc in the original foldl.


-}

math2d :: (MonadFail m, MonadLogger m, MonadState Int m) => [[Int]] -> m ()
math2d [] = return ()
math2d  = f 
  where
    f :: (MonadFail m, MonadLogger m, MonadState Int m) => [[Int]] -> m ()
    f xs = do
      if any (/= length (head xs)) (fmap length (tail xs)) 
        then fail "Sublists are not consistent length."
        else foldM f_vert 1 xs >> return ()
  
    f_vert :: (MonadState Int m, MonadLogger m) => Int -> [Int] -> m (Int)
    f_vert rowNo p = do
      _ <- foldM f_horiz True p
      rowValue <- get 
      logDebugN (pack $  "After row " <> show rowNo <> ", value is " <> show rowValue )
        >> return (rowNo + 1)

    f_horiz :: (MonadState Int m) => Bool -> Int -> m (Bool)
    f_horiz True y = modify (+ y) >> return False
    f_horiz False y = modify (* y) >> return True