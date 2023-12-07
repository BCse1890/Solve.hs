module Main (main) where
import System.Environment

import Data.Maybe (listToMaybe)
import qualified Data.Map as M
import System.Process (callCommand)

baseTestCommand :: String
baseTestCommand = "stack build Solve-hs:test:"

baseBenchCommand :: String
baseBenchCommand = "stack build Solve-hs:bench:"

main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args >>= flip M.lookup testMap of
    Nothing -> listTestSuites
    Just extension -> callCommand (testCommand extension ++ extension)
  where
    testCommand entry = if last entry == 'b'
      then baseBenchCommand
      else baseTestCommand

testMap :: M.Map String String
testMap = M.fromList
  [ ("1-1", "m1l1")
  , ("1-2", "m1l2")
  , ("1-3", "m1l3")
  , ("1-4", "m1l4")
  , ("1-5", "m1l5")
  , ("1-6", "m1l6")
  , ("1-7", "m1l7")
  , ("1-8", "m1l8")
  , ("1-9", "m1l9")
  , ("1-10", "m1l10")
  , ("1-11", "m1l11")
  , ("2-1", "m2l1")
  , ("2-2", "m2l2")
  , ("2-2b", "m2l2b")
  , ("2-3", "m2l3")
  , ("2-4", "m2l4")
  , ("2-5", "m2l5")
  , ("2-6", "m2l6")
  , ("2-6b", "m2l6b")
  , ("2-7", "m2l7")
  , ("2-8", "m2l8")
  , ("2-8b", "m2l8b")
  , ("2-9", "m2l9")
  , ("2-10", "m2l10")
  , ("2-10b", "m2l10b")
  ]

listTestSuites :: IO ()
listTestSuites = do
  putStrLn "Please enter a valid test suite!"
  putStrLn "Use format: {module}-{lecture}, e.g. 1-3, 2-4"
