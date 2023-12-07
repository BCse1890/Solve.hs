module MySet where

data MySet a = MySet (TreeNode a) Int

data TreeNode a = TreeNode

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

insert :: (Ord a) => a -> MySet a -> MySet a
insert a st = undefined

delete :: (Ord a) => a -> MySet a -> MySet a
delete a st = undefined

member :: (Ord a) => a -> MySet a -> Bool
member a st = undefined
