module M2Lecture1 where

import Control.Monad.Fail
import Control.Monad.Logger
import qualified Data.Set as S
import Data.List (sort)

import Utils

compartmentalize :: (MonadFail m) => String -> Int -> m Char
compartmentalize input numBuckets = undefined

traverse2D4P :: Coord2 -> [Direction4] -> Int
traverse2D4P start dirs = undefined

data LightCommand = On | Off | Toggle
  deriving (Show, Eq)

toggleLights :: [(LightCommand, Word, Word)] -> Int
toggleLights = undefined

treeVisibility :: (MonadLogger m) =>
  (Coord2, Coord2, Coord2) -> ([Coord2], [Coord2], [Coord2]) -> m Int
treeVisibility (l1, l2, l3) (tree1s, tree2s, tree3s) = undefined

collectPellets :: (MonadLogger m) => [Coord2] -> [(Direction4, Word)] -> m Int
collectPellets pellets dirs = undefined

gameOfLife :: (MonadLogger m) => Coord2 -> S.Set Coord2 -> m (S.Set Coord2)
gameOfLife (numCols, numRows) aliveSet = undefined
