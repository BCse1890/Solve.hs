{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module M1Lecture9 where

import Control.Monad (foldM)
import Control.Monad.Logger
import Control.Monad.State
import qualified Data.List as L
import Data.Text (pack)

import Utils

commonToken :: (MonadFail m) => [String] -> m String
commonToken = undefined

compartmentalize :: (MonadFail m) => String -> Int -> m Char
compartmentalize input numBuckets = undefined

anyOverlap :: (MonadLogger m) => [(Int, Int)] -> m Bool
anyOverlap intervals = undefined

traverse2D :: (MonadLogger m) => Coord2 -> [Direction4] -> m Int
traverse2D start dirs = undefined

data ListQueue a = ListQueue
  { list1 :: [a]
  , list2 :: [a]
  }

enqueue :: (MonadState (ListQueue a) m) => a -> m ()
enqueue = undefined

dequeue :: (MonadState (ListQueue a) m) => m (Maybe a)
dequeue = undefined

math2d :: (MonadFail m, MonadLogger m, MonadState Int m) => [[Int]] -> m ()
math2d values = undefined
