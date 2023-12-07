{-# LANGUAGE ScopedTypeVariables #-}

module M2Lecture4 where

import Control.Monad.Logger
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.List as L
import qualified Data.MultiMap as MM
import qualified Data.Set as S
import qualified Data.Text as T

import M1Lecture10
import Utils

hiddenMessage :: (MonadLogger m) => [String] -> m String
hiddenMessage inputs = undefined

mazeDrilling :: (MonadLogger m) => Coord2 -> [Coord2] ->
  (Coord2, [Direction4]) -> (Coord2, [Direction4]) -> m (Coord2, Coord2, Coord2)
mazeDrilling (width, height) obstacles (start1, dirs1) (start2, dirs2) = undefined

schoolyardRace :: (MonadLogger m) => Int -> [Coord2] -> [Coord2] -> [Coord2] -> m (Int, Int, Int, Int)
schoolyardRace width team1s team2s obstacles = undefined

type Strike = (Coord2, Int, Direction8)

powerStrikes :: (MonadLogger m) => Coord2 -> [Strike] -> m Int
powerStrikes (numRows, numCols) strikes = undefined

edgePower :: (MonadLogger m) => [(String, String, Int)] -> String -> m Int
edgePower edges start = undefined
