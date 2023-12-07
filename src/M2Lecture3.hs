module M2Lecture3 where

import Control.Monad.Logger
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Ord as O

import M1Lecture10
import Utils

pickBestClump :: [Int] -> Int
pickBestClump values = undefined

directoryCounts :: (MonadLogger m) => [String] -> m [(String, Word)]
directoryCounts inputs = undefined

algaeGrowth :: Int -> Int -> [Int] -> Integer
algaeGrowth days growthRate startingValues = undefined

data SparseMatrix = SparseMatrix
  deriving (Show, Eq)

empty :: Word -> Word -> SparseMatrix
empty numRows numCols = undefined

set :: Word -> Word -> Double -> SparseMatrix -> SparseMatrix
set row col val sparseMatrix = undefined

get :: Word -> Word -> SparseMatrix -> Double
get row col sparseMatrix = undefined

get' :: Word -> Word -> SparseMatrix -> Maybe Double
get' row col sparseMatrix = undefined

add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add matrix1 matrix2 = undefined

add' :: SparseMatrix -> SparseMatrix -> Maybe SparseMatrix
add' matrix1 matrix2 = undefined

multiply :: SparseMatrix -> SparseMatrix -> SparseMatrix
multiply matrix1 matrix2  = undefined

multiply' :: SparseMatrix -> SparseMatrix -> Maybe SparseMatrix
multiply' matrix1 matrix2 = undefined

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)

data Instruction =
  Dep String String Op |
  Assign Int

calculationGraph :: String -> [(String, Instruction)] -> Int
calculationGraph root instrs = undefined
