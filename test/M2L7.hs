module Main where

import Control.Monad.State
import qualified Data.Array as A
import qualified Data.Sequence as Seq
import Test.Tasty
import Test.Tasty.HUnit

import M2Lecture7
import Utils
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 7 Tests"
  [ uniqueCharactersTests
  , skiQueueTests
  , circleStartTests
  , dynamicLeaderboardTests
  , numberShiftTests
  , runSnakeTests
  ]

uniqueCharactersTests :: TestTree
uniqueCharactersTests = testGroup "uniqueCharacters"
  [ testCase "uniqueCharacters 1" $ uniqueCharacters 1 "a" @?= Just 0
  , testCase "uniqueCharacters 2" $ uniqueCharacters 2 "aa" @?= Nothing
  , testCase "uniqueCharacters 3" $ uniqueCharacters 2 "aab" @?= Just 1
  , testCase "uniqueCharacters 4" $ uniqueCharacters 3 "aabbc" @?= Nothing
  , testCase "uniqueCharacters 5" $ uniqueCharacters 4 "abcacfaeepopquxyz"
      @?= Just 4
  , testCase "uniqueCharacters 6" $ uniqueCharacters 5 "abcacfaeepopquxyz"
      @?= Just 10
  , testCase "uniqueCharacters 7" $ uniqueCharacters 7 "abcacfaeepopquxyz"
      @?= Just 10
  , testCase "uniqueCharacters 8" $ uniqueCharacters 8 "abcacfaeepopquxyz"
      @?= Nothing
  , testCase "uniqueCharacters 9" $ uniqueCharacters 8
      "jghdidjskallpeidkbjdjsnamkakdjgibjdakdjakblakqpeifmbkjda"
      @?= Just 11
  , testCase "uniqueCharacters 10" $ uniqueCharacters 10
      "jghdidjskallpeidkbjdjsnamkakdjgibjdakdjakblakqpeifmbkjda"
      @?= Just 41
  , testCase "uniqueCharacters 11" $ uniqueCharacters 11
      "jghdidjskallpeidkbjdjsnamkakdjgibjdakdjakblakqpeifmbkjda"
      @?= Just 45
  , testCase "uniqueCharacters 12" $ uniqueCharacters 12
      "jghdidjskallpeidkbjdjsnamkakdjgibjdakdjakblakqpeifmbkjda"
      @?= Nothing
  ]

skiQueueTests :: TestTree
skiQueueTests = testGroup "skiQueue"
  [ testCase "skiQueue skiers 1" $ runQ (Just $ Skier "J", Just $ Skier "K") $ do
      enqueueSkier (Skier "J")
      enqueueSkier (Skier "K")
      enqueueSkier (Skier "L")
      s1 <- dequeueSkier
      s2 <- dequeueSkier
      return (s1, s2)
  , testCase "skiQueue skiers 2" $ runQ (Just $ Skier "J", Just $ Skier "K", Nothing) $ do
      enqueueSkier (Skier "J")
      s1 <- dequeueSkier
      enqueueSkier (Skier "K")
      s2 <- dequeueSkier
      s3 <- dequeueSkier
      return (s1, s2, s3)
  , testCase "skiQueue snowboarders 1" $ runQ (Just $ Snowboarder "M", Just $ Snowboarder "N") $ do
      enqueueSnowboarder (Snowboarder "M")
      enqueueSnowboarder (Snowboarder "N")
      enqueueSnowboarder (Snowboarder "O")
      s1 <- dequeueSnowboarder
      s2 <- dequeueSnowboarder
      return (s1, s2)
  , testCase "skiQueue snowboarders 2" $
      runQ (Just $ Snowboarder "M", Just $ Snowboarder "N", Nothing) $ do
        enqueueSnowboarder (Snowboarder "M")
        s1 <- dequeueSnowboarder
        enqueueSnowboarder (Snowboarder "N")
        s2 <- dequeueSnowboarder
        s3 <- dequeueSnowboarder
        return (s1, s2, s3)
  , testCase "skiQueue both 1" $ runQ
      ( Just "J", Just $ Snowboarder "M", Just $ Skier "K"
      , Just $ Snowboarder "N", Just $ Snowboarder "O", Just $ Skier "L", Nothing) $ do
        enqueueSkier (Skier "J")
        enqueueSkier (Skier "K")
        enqueueSkier (Skier "L")
        enqueueSnowboarder (Snowboarder "M")
        enqueueSnowboarder (Snowboarder "N")
        enqueueSnowboarder (Snowboarder "O")
        s1 <- dequeueAny
        s2 <- dequeueSnowboarder
        s3 <- dequeueSkier
        s4 <- dequeueSnowboarder
        s5 <- dequeueSnowboarder
        s6 <- dequeueSkier
        s7 <- dequeueAny
        return (s1, s2, s3, s4, s5, s6, s7)
  , testCase "skiQueue both 2" $ runQ
      ( Just "J", Just "M", Just $ Snowboarder "N"
      , Just "K", Just "L", Just $ Snowboarder "O", Nothing) $ do
        enqueueSkier (Skier "J")
        enqueueSnowboarder (Snowboarder "M")
        enqueueSkier (Skier "K")
        enqueueSnowboarder (Snowboarder "N")
        enqueueSkier (Skier "L")
        enqueueSnowboarder (Snowboarder "O")
        s1 <- dequeueAny
        s2 <- dequeueAny
        s3 <- dequeueSnowboarder
        s4 <- dequeueAny
        s5 <- dequeueAny
        s6 <- dequeueSnowboarder
        s7 <- dequeueAny
        return (s1, s2, s3, s4, s5, s6, s7)
  , testCase "skiQueue both 3" $ runQ
      ( Just $ Skier "J", Just $ Skier "K"
      , Just "M", Just "L") $ do
        enqueueSnowboarder (Snowboarder "M")
        enqueueSkier (Skier "J")
        enqueueSkier (Skier "K")
        enqueueSkier (Skier "L")
        enqueueSnowboarder (Snowboarder "N")
        enqueueSnowboarder (Snowboarder "O")
        s1 <- dequeueSkier
        s2 <- dequeueSkier
        s3 <- dequeueAny
        s4 <- dequeueAny
        return (s1, s2, s3, s4)
  ]
  where
    baseQ = SkiQueue Seq.empty Seq.empty Seq.empty
    runQ expectedResults action = do
      actualResults <- evalStateT action baseQ
      actualResults @?= expectedResults

circleStartTests :: TestTree
circleStartTests = testGroup "circleStart"
  [ testCase "circleStart 1" $ shouldReturnLogger (circleStart [(6,40),(5,30)] 6) Nothing
  , testCase "circleStart 2" $ shouldReturnLogger (circleStart [(6,40),(5,30)] 7) (Just 0)
  , testCase "circleStart 3" $ shouldReturnLogger (circleStart [(4,100),(6,100)] 20) (Just 1)
  , testCase "circleStart 4" $ shouldReturnLogger (circleStart [(4,100),(6,100)] 25) (Just 0)
  , testCase "circleStart 5" $ shouldReturnLogger (circleStart [(4,100),(6,100)] 15) Nothing
  , testCase "circleStart 6" $ shouldReturnLogger (circleStart
      [(5,120),(3,83),(5,67),(1,32)] 25) (Just 2)
  , testCase "circleStart 7" $ shouldReturnLogger (circleStart
      [(5,120),(3,83),(5,67),(1,32)] 30) (Just 0)
  , testCase "circleStart 8" $ shouldReturnLogger (circleStart
      [(5,120),(3,83),(5,67),(1,32)] 20) Nothing
  , testCase "circleStart 9" $ shouldReturnLogger (circleStart
      [(15,68),(19,76),(14,56),(19,62),(28,46),(23,54),(36,76),(36,86),(24,47),(24,86),
       (13,90),(1,97)] 3) Nothing
  , testCase "circleStart 10" $ shouldReturnLogger (circleStart
      [(15,68),(19,76),(14,56),(19,62),(28,46),(23,54),(36,76),(36,86),(24,47),(24,86),
       (13,90),(1,97)] 4) (Just 1)
  , testCase "circleStart 11" $ shouldReturnLogger (circleStart
      [(15,68),(19,76),(1,97),(14,56),(19,62),(28,46),(23,54),(36,76),(36,86),(24,47),(24,86),
       (13,90),(1,97)] 4) (Just 3)
  ]

dynamicLeaderboardTests :: TestTree
dynamicLeaderboardTests = testGroup "dynamicLeaderboard"
  [ testCase "dynamicLeaderboard 1" $ shouldReturnLogger (dynamicLeaderboard [500] []) 0
  , testCase "dynamicLeaderboard 2" $ shouldReturnLogger
      (dynamicLeaderboard [500] [400,600,200,800]) 2500
  , testCase "dynamicLeaderboard 3" $ shouldReturnLogger (dynamicLeaderboard [500,300]
      [400,600,200,800]) 4500
  , testCase "dynamicLeaderboard 4" $ shouldReturnLogger (dynamicLeaderboard
      [500,300,300,200,100] [400,600,200,300,400]) 10200
  , testCase "dynamicLeaderboard 5" $ shouldReturnLogger (dynamicLeaderboard
      [36,27,27,9,6,2,1,1] [3,24,54]) 431
  , testCase "dynamicLeaderboard 6" $ shouldReturnLogger (dynamicLeaderboard
      [36,27,27,9,6,2,1,1] [3,24,54,74,20,5,25,83,97,100,27,105,72]) 4483
  ]

numberShiftTests :: TestTree
numberShiftTests = testGroup "numberShift"
  [ testCase "numberShift 1" $ shouldReturnLogger (numberShift [1,3,4,-2]) (-8)
  , testCase "numberShift 2" $ shouldReturnLogger (numberShift [4]) 16
  , testCase "numberShift 3" $ shouldReturnLogger (numberShift [1,2,3,4,5,6]) 15
  , testCase "numberShift 4" $ shouldReturnLogger (numberShift [1,2,3,4,7,6]) 12
  , testCase "numberShift 5" $ shouldReturnLogger (numberShift [1,2,-3,4,5,6]) (-3)
  , testCase "numberShift 6" $ shouldReturnLogger (numberShift [1,-2,3,4,-5,6]) 12
  , testCase "numberShift 7" $ shouldReturnLogger (numberShift
      [6, 2, 16, 6, 7, 2, 10, 5, 4, 20, 31, 17, 5]) 4
  , testCase "numberShift 8" $ shouldReturnLogger (numberShift
      [6, 2, 16, -6, 7, 2, 10, 5, 4, 20, 31, 17, -5]) 70
  ]

runSnakeTests :: TestTree
runSnakeTests = testGroup "runSnake"
  [ testCase "runSnake 1" $ shouldReturnLogger (runSnake
      [(TurnRight, 5), (TurnLeft, 10)] [(14,12), (17,13)]) (2, True)
  , testCase "runSnake 2" $ shouldReturnLogger (runSnake
      [(TurnLeft, 15), (TurnLeft, 5)] [(0,11)]) (0, False)
  , testCase "runSnake 3" $ shouldReturnLogger (runSnake
      [(TurnRight, 15), (TurnRight, 5)] [(24,11)]) (0, False)
  , testCase "runSnake 4" $ shouldReturnLogger (runSnake
      [(TurnRight, 5), (TurnLeft, 15)] [(14,12), (17,13)]) (2, False)
  , testCase "runSnake 5" $ shouldReturnLogger (runSnake
      [(TurnRight, 5), (TurnRight, 3), (TurnRight, 4), (TurnLeft, 10)]
      [(13,0), (12,5), (12,0), (14,0), (14,12), (17,10), (13,5), (15,9)]) (5, False)
  , testCase "runSnake 6" $ shouldReturnLogger (runSnake
      [(TurnRight, 5),(TurnLeft,1),(TurnLeft,1),(TurnLeft,1)]
      [(13,12), (14,12), (15,12)]) (3, False)
  , testCase "runSnake 7" $ shouldReturnLogger (runSnake
      [ (TurnRight, 12),(TurnLeft,12),(TurnLeft,24)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      ]
      [(x,y) | x <- [0,2..22], y <- [24,23..17]]) (96, True)
  , testCase "runSnake 7" $ shouldReturnLogger (runSnake
      [ (TurnRight, 12),(TurnLeft,12),(TurnLeft,24)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnLeft, 1), (TurnLeft, 23)
      , (TurnRight, 1), (TurnRight,23)
      , (TurnRight, 1), (TurnRight, 23)
      ]
      [(x,y) | x <- [0,2..22], y <- [24,23..17]]) (96, False)
  ]
