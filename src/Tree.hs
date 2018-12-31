module Tree
( singleton
, treeInsert
, size
, maximum'
, depth
, map'
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

-- From "Functional Programming in Scala"
-- Exercise 25
-- Write a function size that counts the number of nodes in a tree.
size :: Tree a -> Int
size EmptyTree = 0
size (Node _ left right) = 1 + size left + size right

-- Exercise 26
-- Write a function maximum that return the maximum element in a Tree[Int]
maximum' :: (Ord a) => Tree a -> a
maximum' EmptyTree = error "Error empty tree"
maximum' (Node x EmptyTree EmptyTree) = x
maximum' (Node x left EmptyTree) = maximum' left
maximum' (Node x EmptyTree right) = maximum' right
maximum' (Node x left right) = maximum [maximum' left, maximum' right]

-- Exercise 27
-- Write a function depth that returns the maximum path length from the
-- root of a tree to any leaf
depth :: Tree a -> Int
depth EmptyTree = 0
depth (Node _ leaf right) = maximum [depth leaf, depth right] + 1

-- Exercise 28
-- Write a function map, that modifies each element in a tree with
-- a given function
map' :: Tree a -> (a -> b) -> Tree b
map' EmptyTree _ = EmptyTree
map' (Node x left right) f = Node (f x) (map' left f) (map' right f)

instance Functor Tree where
    fmap f root = map' root f