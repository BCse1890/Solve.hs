{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module BalancedSet where

data MySet a = MySet (TreeNode a) Int 
  deriving (Show, Eq)

data TreeNode a = TreeNode
  deriving (Show, Eq)

empty :: MySet a
empty = undefined

singleton :: (Ord a) => a -> MySet a
singleton a = undefined

fromList :: (Ord a) => [a] -> MySet a
fromList = undefined

toList :: (Ord a) => MySet a -> [a]
toList st = undefined

null :: MySet a -> Bool
null st = undefined

size :: MySet a -> Int
size st = undefined

member :: (Ord a) => a -> MySet a -> Bool
member a st = undefined

nodeHeight :: TreeNode a -> Int
nodeHeight node = undefined

nodeVal :: TreeNode a -> a
nodeVal node = undefined

nodeBalance :: TreeNode a -> Int
nodeBalance node = undefined

balanceNode :: TreeNode a -> TreeNode a
balanceNode node = undefined

findMinVal :: TreeNode a -> a
findMinVal node = undefined

rotateLeft :: TreeNode a -> TreeNode a
rotateLeft node = undefined

rotateRight :: TreeNode a -> TreeNode a
rotateRight node = undefined

insert :: (Ord a) => a -> MySet a -> MySet a
insert a st = undefined

insertNode :: (Ord a) => a -> TreeNode a -> TreeNode a
insertNode a node = undefined

delete :: (Ord a) => a -> MySet a -> MySet a
delete a st = undefined

deleteNode :: (Ord a) => a -> TreeNode a -> TreeNode a
deleteNode a node = undefined
