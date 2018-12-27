module Tree
( singleton
, treeInsert
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
    | x == y = Node x left right
    | x < y  = Node y (treeInsert x left) right
    | x > y  = Node y left (treeInsert x right)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
    | x == y = True
    | x < y  = treeElem x left
    | x > y  = treeElem x right