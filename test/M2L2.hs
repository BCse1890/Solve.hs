{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Control.Monad (foldM, void)
import System.Random.Shuffle (shuffle')
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

import qualified BalancedSet as S

main :: IO ()
main = defaultMain $ testGroup "Module 2 Lecture 2 Tests"
  [ nodeHeightTests
  , nodeValTests
  , nodeBalanceTests
  , balanceNodeTests
  , findMinValTests
  , rotateLeftTests
  , rotateRightTests
  , insertTests
  , deleteTests
  , nullTests
  , sizeTests
  , toListTests
  , memberTests
  , balanceTests
  ]

nodeHeightTests :: TestTree
nodeHeightTests = testGroup "nodeHeight"
  [ testCase "nodeHeight 1" $ S.nodeHeight S.Nil @?= 0
  , testCase "nodeHeight 2" $ S.nodeHeight (S.Node 5 S.Nil S.Nil 0 1) @?= 1
  , testCase "nodeHeight 3" $ S.nodeHeight (S.Node 5 (S.Node 3 S.Nil S.Nil 0 1) S.Nil 1 2) @?= 2
  , testCase "nodeHeight 4" $ S.nodeHeight (S.Node 5 S.Nil (S.Node 7 S.Nil S.Nil 0 1) (-1) 2) @?= 2
  , testCase "nodeHeight 5" $ S.nodeHeight (S.Node 5 (S.Node 3 S.Nil S.Nil 0 1) (S.Node 7 S.Nil S.Nil 0 1) 0 2) @?= 2
  ]

nodeValTests :: TestTree
nodeValTests = testGroup "nodeVal"
  [ testCase "nodeVal 1" $ S.nodeVal (S.Node 5 S.Nil S.Nil 0 1) @?= 5
  , testCase "nodeVal 2" $ S.nodeVal (S.Node 8 (S.Node 3 S.Nil S.Nil 0 1) S.Nil 1 2) @?= 8
  , testCase "nodeVal 3" $ S.nodeVal (S.Node 2 S.Nil (S.Node 7 S.Nil S.Nil 0 1) (-1) 2) @?= 2
  , testCase "nodeVal 4" $ S.nodeVal (S.Node 4 (S.Node 3 S.Nil S.Nil 0 1) (S.Node 7 S.Nil S.Nil 0 1) 0 2) @?= 4
  ]

nodeBalanceTests :: TestTree
nodeBalanceTests = testGroup "nodeBalance"
  [ testCase "nodeBalance 1" $ S.nodeBalance S.Nil @?= 0
  , testCase "nodeBalance 2" $ S.nodeBalance (S.Node 5 S.Nil S.Nil 0 1) @?= 0
  , testCase "nodeBalance 3" $ S.nodeBalance (S.Node 8 (S.Node 3 S.Nil S.Nil 0 1) S.Nil 1 2) @?= 1
  , testCase "nodeBalance 4" $ S.nodeBalance (S.Node 2 S.Nil (S.Node 7 S.Nil S.Nil 0 1) (-1) 2) @?= (-1)
  , testCase "nodeBalance 5" $ S.nodeBalance (S.Node 4 (S.Node 3 S.Nil S.Nil 0 1) (S.Node 7 S.Nil S.Nil 0 1) 0 2) @?= 0
  , testCase "nodeBalance 6" $ S.nodeBalance (S.Node 4 (S.Node 3 (S.Node 1 S.Nil S.Nil 0 1) S.Nil 1 2) S.Nil 2 2) @?= 2
  ]

balanceNodeTests :: TestTree
balanceNodeTests = testGroup "balanceNode"
  [ testCase "balanceNode 1" $ S.balanceNode (S.Nil :: S.TreeNode Int) @?= S.Nil
  , testCase "balanceNode 2" $ S.balanceNode (S.Node 3 S.Nil S.Nil 5 5) @?=
      (S.Node 3 S.Nil S.Nil 0 1)
  , testCase "balanceNode 3" $ bh (S.balanceNode (S.Node 3 (S.Node 1 S.Nil S.Nil 0 1) S.Nil 5 5) )
      @?= (1, 2)
  , testCase "balanceNode 4" $ bh (S.balanceNode
      (S.Node 3 (S.Node 1 (S.Node 0 S.Nil S.Nil 0 1) S.Nil 1 2) S.Nil 5 5))
      @?= (2, 3)
  , testCase "balanceNode 5" $ bh (S.balanceNode
      (S.Node 3 (S.Node 1 S.Nil (S.Node 1 S.Nil S.Nil 0 1) (-1) 2) S.Nil 5 5))
      @?= (2, 3)
  , testCase "balanceNode 6" $ bh (S.balanceNode
      (S.Node 3 (S.Node 1 S.Nil (S.Node 1 S.Nil S.Nil 0 1) (-1) 2) (S.Node 5 S.Nil S.Nil 0 1) 5 5))
      @?= (1, 3)
  , testCase "balanceNode 7" $ bh (S.balanceNode
      (S.Node 3 (S.Node 1 S.Nil (S.Node 1 S.Nil S.Nil 0 1) (-1) 2)
        (S.Node 5 (S.Node 4 S.Nil S.Nil 0 1) S.Nil 1 2) 5 5))
      @?= (0, 3)
  , testCase "balanceNode 8" $ bh (S.balanceNode
      (S.Node 3 (S.Node 1 S.Nil (S.Node 1 S.Nil S.Nil 0 1) (-1) 2)
        (S.Node 5 (S.Node 4 S.Nil S.Nil 0 1) (S.Node 7 S.Nil S.Nil 0 1) 0 2) 5 5))
      @?= (0, 3)
  , testCase "balanceNode 9" $ bh (S.balanceNode
      (S.Node 3 (S.Node 1 S.Nil (S.Node 1 S.Nil S.Nil 0 1) (-1) 2)
        (S.Node 5 S.Nil (S.Node 7 (S.Node 6 S.Nil S.Nil 0 1) S.Nil (-1) 2) (-2) 3) 5 5))
      @?= (-1, 4)
  ]
  where
    bh (S.Node _ _ _ b h) = (b, h)

findMinValTests :: TestTree
findMinValTests = testGroup "findMinVal"
  [ testCase "findMinVal 1" $ S.findMinVal (S.Node 13 S.Nil S.Nil 0 1) @?= 13
  , testCase "findMinVal 2" $ S.findMinVal
      (S.Node 13 (S.Node 11 S.Nil S.Nil 0 1) S.Nil 1 2) @?= 11
  , testCase "findMinVal 3" $ S.findMinVal
      (S.Node 13 S.Nil (S.Node 15 S.Nil S.Nil 0 1) (-1) 2) @?= 13
  , testCase "findMinVal 4" $ S.findMinVal
      (S.Node 13 (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) (S.Node 12 S.Nil S.Nil 0 1) 0 2) S.Nil 2 3) @?= 10
  , testCase "findMinVal 5" $ S.findMinVal
      (S.Node 13 (S.Node 11 S.Nil (S.Node 12 S.Nil S.Nil 0 1) (-1) 2) S.Nil 2 3) @?= 11
  , testCase "findMinVal 6" $ S.findMinVal
      (S.Node 13 S.Nil (S.Node 15 (S.Node 14 S.Nil S.Nil 0 1) S.Nil 1 2) (-2) 3) @?= 13
  ]

rotateLeftTests :: TestTree
rotateLeftTests = testGroup "rotateLeft"
  [ testCase "rotateLeft 1" $ S.rotateLeft (S.Nil :: S.TreeNode Int) @?= S.Nil
  , testCase "rotateLeft 2" $ S.rotateLeft tree1 @?= tree1
  , testCase "rotateLeft 3" $ S.rotateLeft tree5 @?= tree5
  , testCase "rotateLeft 4" $ S.rotateLeft tree6 @?=
      (S.Node 15 (S.Node 13 S.Nil S.Nil 0 1) S.Nil 1 2)
  , testCase "rotateLeft 5" $ S.rotateLeft tree7 @?=
      (S.Node 15 (S.Node 13 S.Nil (S.Node 14 S.Nil S.Nil 0 1) (-1) 2) S.Nil 2 3)
  , testCase "rotateLeft 6" $ S.rotateLeft tree8 @?=
      (S.Node 15 (S.Node 13 S.Nil S.Nil 0 1) (S.Node 16 S.Nil S.Nil 0 1) 0 2)
  , testCase "rotateLeft 7" $ S.rotateLeft tree9 @?=
      (S.Node 15 (S.Node 13 S.Nil (S.Node 14 S.Nil S.Nil 0 1) (-1) 2) (S.Node 16 S.Nil S.Nil 0 1) 1 3)
  , testCase "rotateLeft 8" $ S.rotateLeft tree10 @?=
      (S.Node 15
        (S.Node 13
          (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) (S.Node 12 S.Nil S.Nil 0 1) 0 2)
          (S.Node 14 S.Nil S.Nil 0 1) 1 3)
        (S.Node 16 S.Nil S.Nil 0 1)
        2 4)
  ]

rotateRightTests :: TestTree
rotateRightTests = testGroup "rotateRight"
  [ testCase "rotateRight 1" $ S.rotateRight (S.Nil :: S.TreeNode Int) @?= S.Nil
  , testCase "rotateRight 2" $ S.rotateRight tree1 @?= tree1
  , testCase "rotateRight 3" $ S.rotateRight tree9 @?= tree9
  , testCase "rotateRight 4" $ S.rotateRight tree2 @?=
      (S.Node 11 S.Nil (S.Node 13 S.Nil S.Nil 0 1) (-1) 2)
  , testCase "rotateRight 5" $ S.rotateRight tree4 @?=
      (S.Node 11 S.Nil (S.Node 13 (S.Node 12 S.Nil S.Nil 0 1) S.Nil 1 2) (-2) 3)
  , testCase "rotateRight 6" $ S.rotateRight tree3 @?=
      (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) (S.Node 13 S.Nil S.Nil 0 1) 0 2)
  , testCase "rotateRight 7" $ S.rotateRight tree5 @?=
      (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) (S.Node 13 (S.Node 12 S.Nil S.Nil 0 1) S.Nil 1 2) (-1) 3)
  , testCase "rotateRight 8" $ S.rotateRight tree10 @?=
      (S.Node 11
        (S.Node 10 S.Nil S.Nil 0 1)
        (S.Node 13
          (S.Node 12 S.Nil S.Nil 0 1)
          (S.Node 15 (S.Node 14 S.Nil S.Nil 0 1) (S.Node 16 S.Nil S.Nil 0 1) 0 2)
           (-1) 3)
        (-2) 4)
  ]

insertTests :: TestTree
insertTests = testGroup "insert"
  [ testCase "insert 1" $ S.insertNode 22 tree11 @?=
      (S.Node 20
        (S.Node 13 (S.Node 8 S.Nil S.Nil 0 1) (S.Node 17 S.Nil S.Nil 0 1) 0 2)
        (S.Node 24 (S.Node 22 S.Nil S.Nil 0 1) S.Nil 1 2)
      0 3)
  , testCase "insert 2" $ S.insertNode 18 tree11 @?=
      (S.Node 17
        (S.Node 13 (S.Node 8 S.Nil S.Nil 0 1) S.Nil 1 2)
        (S.Node 20 (S.Node 18 S.Nil S.Nil 0 1) (S.Node 24 S.Nil S.Nil 0 1) 0 2)
      0 3)
  , testCase "insert 3" $ S.insertNode 1 tree12 @?=
      (S.Node 8
        (S.Node 4 (S.Node 1 S.Nil S.Nil 0 1) S.Nil 1 2)
        (S.Node 13 (S.Node 11 S.Nil S.Nil 0 1) (S.Node 20 S.Nil S.Nil 0 1) 0 2)
      0 3)
  , testCase "insert 4" $ S.insertNode 10 tree12 @?=
      (S.Node 11
        (S.Node 8 (S.Node 4 S.Nil S.Nil 0 1) (S.Node 10 S.Nil S.Nil 0 1) 0 2)
        (S.Node 13 S.Nil (S.Node 20 S.Nil S.Nil 0 1) (-1) 2)
      0 3)
  , testCase "insert 5" $ S.insertNode 20 tree12 @?= tree12
  , testCase "insert 6" $ S.insertNode 19 tree12 @?=
      (S.Node 13
        (S.Node 8 (S.Node 4 S.Nil S.Nil 0 1) (S.Node 11 S.Nil S.Nil 0 1) 0 2)
        (S.Node 20 (S.Node 19 S.Nil S.Nil 0 1) S.Nil 1 2)
        0 3)
  ]

deleteTests :: TestTree
deleteTests = testGroup "delete"
  [ testCase "delete 1" $ S.deleteNode 8 tree11 @?=
      (S.Node 20
        (S.Node 13 S.Nil (S.Node 17 S.Nil S.Nil 0 1) (-1) 2)
        (S.Node 24 S.Nil S.Nil 0 1)
        1 3)
  , testCase "delete 2" $ S.deleteNode 20 tree12 @?=
      (S.Node 8
        (S.Node 4 S.Nil S.Nil 0 1)
        (S.Node 13 (S.Node 11 S.Nil S.Nil 0 1) S.Nil 1 2)
        (-1) 3)
  , testCase "delete 3" $ S.deleteNode 8 tree13 @?=
      (S.Node 17
        (S.Node 13 (S.Node 4 S.Nil S.Nil 0 1) S.Nil 1 2)
        (S.Node 20 (S.Node 18 S.Nil S.Nil 0 1) (S.Node 24 S.Nil S.Nil 0 1) 0 2)
        0 3)
  , testCase "delete 4" $ S.deleteNode 20 tree14 @?=
      (S.Node 11
        (S.Node 8 (S.Node 4 S.Nil S.Nil 0 1) (S.Node 10 S.Nil S.Nil 0 1) 0 2)
        (S.Node 13 S.Nil (S.Node 25 S.Nil S.Nil 0 1) (-1) 2)
        0 3)
  , testCase "delete 5" $ S.deleteNode 8 tree15 @?=
      (S.Node 13
        (S.Node 9
          (S.Node 4 (S.Node 1 S.Nil S.Nil 0 1) S.Nil 1 2)
          (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) (S.Node 12 S.Nil S.Nil 0 1) 0 2)
          0 3)
        (S.Node 20
          (S.Node 15 S.Nil S.Nil 0 1) 
          (S.Node 30 (S.Node 25 S.Nil S.Nil 0 1) (S.Node 40 S.Nil S.Nil 0 1) 0 2)
          (-1) 3)
        0 4)
  ]

-- One root node
tree1 :: S.TreeNode Int
tree1 = S.Node 13 S.Nil S.Nil 0 1

-- One left child
tree2 :: S.TreeNode Int
tree2 = (S.Node 13 (S.Node 11 S.Nil S.Nil 0 1) S.Nil 1 2)

-- Left-Left Child (unbalanced)
tree3 :: S.TreeNode Int
tree3 = (S.Node 13 (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) S.Nil 1 2) S.Nil 2 3)

-- Left-Right Child (unbalanced)
tree4 :: S.TreeNode Int
tree4 = (S.Node 13 (S.Node 11 S.Nil (S.Node 12 S.Nil S.Nil 0 1) (-1) 2) S.Nil 2 3)

-- Left-Both Children (unbalanced)
tree5 :: S.TreeNode Int
tree5 = (S.Node 13
  (S.Node 11
    (S.Node 10 S.Nil S.Nil 0 1) (S.Node 12 S.Nil S.Nil 0 1) 0 2)
    S.Nil 2 3)

-- One right child
tree6 :: S.TreeNode Int
tree6 = (S.Node 13 S.Nil (S.Node 15 S.Nil S.Nil 0 1) (-1) 2)

-- Right-Left Child (unbalanced)
tree7 :: S.TreeNode Int
tree7 = (S.Node 13 S.Nil (S.Node 15 (S.Node 14 S.Nil S.Nil 0 1) S.Nil 1 2) (-2) 3)

-- Right-Right Child (unbalanced)
tree8 :: S.TreeNode Int
tree8 = (S.Node 13 S.Nil (S.Node 15 S.Nil (S.Node 16 S.Nil S.Nil 0 1) (-1) 2) (-2) 3)

-- Right-Both Children (unbalanced)
tree9 :: S.TreeNode Int
tree9 = (S.Node 13 S.Nil (S.Node 15 (S.Node 14 S.Nil S.Nil 0 1) (S.Node 16 S.Nil S.Nil 0 1) 0 2) (-2) 3)

-- Both-Both Children (balanced)
tree10 :: S.TreeNode Int
tree10 = (S.Node 13
  (S.Node 11 (S.Node 10 S.Nil S.Nil 0 1) (S.Node 12 S.Nil S.Nil 0 1) 0 2)
  (S.Node 15 (S.Node 14 S.Nil S.Nil 0 1) (S.Node 16 S.Nil S.Nil 0 1) 0 2)
  0 3)

-- Left Empty, Right Both Children
tree11 :: S.TreeNode Int
tree11 = (S.Node 13
  (S.Node 8 S.Nil S.Nil 0 1)
  (S.Node 20 (S.Node 17 S.Nil S.Nil 0 1) (S.Node 24 S.Nil S.Nil 0 1) 0 2)
  (-1) 3)

-- Left Both, Right Empty (balanced)
tree12 :: S.TreeNode Int
tree12 = (S.Node 13
  (S.Node 8 (S.Node 4 S.Nil S.Nil 0 1) (S.Node 11 S.Nil S.Nil 0 1) 0 2)
  (S.Node 20 S.Nil S.Nil 0 1)
  1 3)

tree13 :: S.TreeNode Int
tree13 = S.insertNode 18 (S.insertNode 4 tree11)

tree14 :: S.TreeNode Int
tree14 = S.insertNode 10 (S.insertNode 25 tree12)

tree15 :: S.TreeNode Int
tree15 = foldr S.insertNode tree12 [10,1,9,12,4,11,25,40,15,30]

nullTests :: TestTree
nullTests = testGroup "null"
  [ testCase "null 1" $ S.null S.empty @?= True
  , testCase "null 2" $ S.null (S.singleton 1) @?= False
  , testCase "null 3" $ S.null (S.fromList [1, 2]) @?= False
  , testCase "null 4" $ S.null (S.insert "Hi" S.empty) @?= False
  , testCase "null 5" $ S.null (S.delete "Hi" (S.singleton "Hi")) @?= True
  ]

sizeTests :: TestTree
sizeTests = testGroup "size"
  [ testCase "size 1" $ S.size S.empty @?= 0
  , testCase "size 2" $ S.size (S.singleton 1) @?= 1
  , testCase "size 3" $ S.size (S.fromList [1, 2, 5, 5]) @?= 3
  , testCase "size 4" $ S.size (S.insert "Hi" S.empty) @?= 1
  , testCase "size 5" $ S.size (S.insert "Hi" (S.singleton "Hi")) @?= 1
  , testCase "size 6" $ S.size (S.delete "Hi" (S.singleton "Hi")) @?= 0
  ]

toListTests :: TestTree
toListTests = testGroup "toList"
  [ testCase "toList 1" $ S.toList S.empty @?= ([] :: [Int])
  , testCase "toList 2" $ S.toList (S.singleton 1) @?= [1]
  , testCase "toList 3" $ S.toList (S.fromList [1, 2, 5, 5]) @?= [1, 2, 5]
  , testCase "toList 4" $ S.toList (S.insert "Hi" (S.singleton "Hi")) @?= ["Hi"]
  , testCase "toList 5" $ S.toList (S.delete "Hi" (S.singleton "Hi")) @?= []
  , testCase "toList 6" $ S.toList
      (S.insert 10 (S.insert 2 (S.insert 4 (S.insert 7 (S.insert 8 (S.fromList [3, 9, 5, 1, 6]))))))
      @?= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  ]

memberTests :: TestTree
memberTests = testGroup "member"
  [ testCase "member 1" $ S.member 1 S.empty @?= False
  , testCase "member 2" $ S.member 1 (S.singleton 1) @?= True
  , testCase "member 3" $ S.member 2 (S.singleton 1) @?= False
  , testCase "member 4" $ S.member 2 (S.fromList [1, 2, 5, 5]) @?= True
  , testCase "member 5" $ S.member 5 (S.fromList [1, 2, 5, 5]) @?= True
  , testCase "member 6" $ S.member 6 (S.fromList [1, 2, 5, 5]) @?= False
  , testCase "member 7" $ S.member 5 (S.delete 5 (S.fromList [1, 2, 5, 5])) @?= False
  , testCase "member 8" $ S.member "Hi" (S.insert "Hi" (S.singleton "Hi")) @?= True
  , testCase "member 9" $ S.member "Hi" (S.delete "Hi" (S.singleton "Hi")) @?= False
  ]

balanceTests :: TestTree
balanceTests = testGroup "balance"
  [ testCase "increasing order" (validateInsertAndDeletes [1..100])
  , testCase "decreasing order" (validateInsertAndDeletes [100,99..1])
  -- If the arbitrary cases are giving you trouble,
  -- try printing the list (uncomment line in 'generateRandomList' below)
  -- and then you can walk through the insertions in GHCI
  , testCase "arbitrary order 1" (generateRandomList 152 >>= validateInsertAndDeletes)
  , testCase "arbitrary order 2" (generateRandomList 486 >>= validateInsertAndDeletes)
  , testCase "arbitrary order 3" (generateRandomList 762 >>= validateInsertAndDeletes)
  ]

generateRandomList :: Int -> IO [Int]
generateRandomList i = do
  let g = mkStdGen i
  let l = shuffle' [1..100] 100 g
  -- print l
  return l

validateInsertAndDeletes :: [Int] -> Assertion
validateInsertAndDeletes values = do
  finalSet <- foldM insertAndValidate S.empty values
  void $ foldM deleteAndValidate finalSet values
  where
    insertAndValidate prevSet i = do
      let newSet = S.insert i prevSet
      assertBool ("Inserted " ++ show i ++ " but not present in set!") (S.member i newSet)
      assertBool ("Inserted " ++ show i ++ " but no longer balanced!")
        (validateHeightAndBalance newSet)
      return newSet
    deleteAndValidate prevSet i = do
      let newSet = S.delete i prevSet
      assertBool ("Deleted " ++ show i ++ " but present in set!") (not $ S.member i newSet)
      assertBool ("Deleted " ++ show i ++ " but no longer balanced!")
        (validateHeightAndBalance newSet)
      return newSet

validateHeightAndBalance :: S.MySet a -> Bool
validateHeightAndBalance (S.MySet root _) = fst (helper root)
  where
    helper :: S.TreeNode a -> (Bool, Int)
    helper S.Nil = (True, 0)
    helper (S.Node _ S.Nil S.Nil b h) = (b == 0 && h == 1, 1)
    helper (S.Node _ (S.Node _ S.Nil S.Nil _ _) S.Nil b h) = (b == 1 && h == 2, 2)
    helper (S.Node _ S.Nil (S.Node _ S.Nil S.Nil _ _) b h) = (b == -1 && h == 2, 2)
    helper (S.Node _ l@(S.Node _ _ _ _ _) r@(S.Node _ _ _ _ _) b h) =
      let (lValid, lHeight) = helper l
          (rValid, rHeight) = helper r
      in  (lValid && rValid && b == (lHeight - rHeight) && h == 1 + max lHeight rHeight, h)
    helper _ = (False, -1)
