{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module M1Lecture11 where

import Control.Monad (foldM)
import Control.Monad.Logger (MonadLogger)
import Data.List (sortOn, sort, nub, elemIndices)

countIntervalsExcludingSpikes :: (MonadLogger m) => [(Int, Int)] -> [Int] -> m Int
countIntervalsExcludingSpikes intervals spikes = undefined

backpackMeals :: (MonadLogger m) => Int -> [Int] -> [Int] -> m Int
backpackMeals maxWeight breakfasts lunches = undefined

stackBoxes :: (MonadLogger m) => [String] -> [(Int, Int, Int)] -> m String
stackBoxes stacks moves = undefined

data Register = Eax | Ebx | Ecx
  deriving (Show, Eq, Enum)

data Command =
  LoadValue Register Int |
  -- First register is destination
  -- e.g. AddRegister Eax Ebx ==> Eax += Ebx
  AddRegister Register Register |
  SubRegister Register Register |
  MultRegister Register Register |
  PrintRegister Register

runAssembly :: (MonadLogger m) => [Command] -> m [Int]
runAssembly commands = undefined

wordSearch :: (MonadLogger m) => String -> [[Char]] -> m Bool
wordSearch input grid = undefined

hiddenMessage :: (MonadLogger m) => [String] -> m String
hiddenMessage inputs = undefined
