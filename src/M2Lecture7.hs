module M2Lecture7 where

import Control.Monad.Logger
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Utils

uniqueCharacters :: Int -> String -> Maybe Int
uniqueCharacters x input = undefined

newtype Skier = Skier
  { unSkier :: String }
  deriving (Show, Eq)

newtype Snowboarder = Snowboarder
  { unSnowboarder :: String }
  deriving (Show, Eq)

data SkiQueue = SkiQueue
  { q1 :: Seq.Seq Skier
  , q2 :: Seq.Seq Snowboarder
  , q3 :: Seq.Seq ()
  }

enqueueSkier :: (Monad m) => Skier -> StateT SkiQueue m ()
enqueueSkier skier = undefined

enqueueSnowboarder :: (Monad m) => Snowboarder -> StateT SkiQueue m ()
enqueueSnowboarder snowboarder = undefined

dequeueSkier :: (Monad m) => StateT SkiQueue m (Maybe Skier)
dequeueSkier = undefined

dequeueSnowboarder :: (Monad m) => StateT SkiQueue m (Maybe Snowboarder)
dequeueSnowboarder = undefined

dequeueAny :: (Monad m) => StateT SkiQueue m (Maybe String)
dequeueAny = undefined

circleStart :: (MonadLogger m) => [(Int, Int)] -> Int -> m (Maybe Int)
circleStart towns mpg = undefined

dynamicLeaderboard :: (MonadLogger m) => [Int] -> [Int] -> m Int
dynamicLeaderboard initialLeaderboard newScores = undefined

numberShift :: (MonadLogger m) => [Int] -> m Int
numberShift values = undefined

runSnake :: (MonadLogger m) => [(Turn, Int)] -> [Coord2] -> m (Int, Bool)
runSnake directions pellets = undefined
