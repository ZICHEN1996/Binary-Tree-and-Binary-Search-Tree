-- Remember to import the binary tree module
module BinarySearchTree (
    BinarySearchTree,
    treeIsValid,        -- :: (Ord a) => BinarySearchTree a -> Bool
    treeMinimum,        -- :: (Ord a) => BinarySearchTree a -> a
    treeMaximum,        -- :: (Ord a) => BinarySearchTree a -> a
    treeContains,       -- :: (Ord a) => a -> BinarySearchTree a -> Bool
    treeInsert,         -- :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
    treeFlattenOrdered,  -- :: (Ord a) => BinarySearchTree a -> [a]
    treeBalance
) where

import BinaryTree
import Data.List

type BinarySearchTree a = BinaryTree a
treeIsValid
  :: Ord a
  => BinarySearchTree a -> Bool
treeIsValid Null = True
treeIsValid (Node n Null Null) = True
treeIsValid (Node n Null t) = minimum (treeFlatten t) > n && treeIsValid t
treeIsValid (Node n t Null) = maximum (treeFlatten t) < n && treeIsValid t
treeIsValid (Node n t1 t2) = maximum (treeFlatten t1) < n  && minimum (treeFlatten t2) > n && treeIsValid t1 && treeIsValid t2
-- Another solution: treeIsValid tree = isIncreasing (treeFlatten tree) where isIncreasing .....
treeMinimum
  :: Ord a
  => BinarySearchTree a -> a
treeMinimum Null = error "Tree is empty"
treeMinimum (Node n Null _) = n
treeMinimum (Node _ t _) = treeMinimum t
treeMaximum
  :: Ord a
  => BinarySearchTree a -> a
treeMaximum Null = error "Tree is empty"
treeMaximum (Node n _ Null) = n
treeMaximum (Node _ _ t) = treeMaximum t
treeContains
  :: Ord a
  => a -> BinarySearchTree a -> Bool
treeContains _ Null = False
treeContains x (Node n t1 t2)
    | x == n = True
    | x < n = treeContains x t1
    | x > n = treeContains x t2
treeFlattenOrdered
  :: Ord a
  => BinarySearchTree a -> [a]
treeFlattenOrdered Null = []
treeFlattenOrdered (Node n t1 t2) = treeFlattenOrdered t1 ++ [n] ++ treeFlattenOrdered t2
treeInsert
  :: Ord a
  => a -> BinarySearchTree a -> BinarySearchTree a
treeInsert x Null = Node x Null Null
treeInsert x (Node n t1 t2)
    | x == n = Node n t1 t2
    | x < n = Node n (treeInsert x t1) t2
    | x > n = Node n t1 (treeInsert x t2)
test4 :: BinarySearchTree Char
test4 = Node 'g' (Node 'c' (Node 'a' Null (Node 'b' Null Null))
    (Node 'e' (Node 'f' Null Null) (Node 'g' Null Null)))
    (Node 'i' (Node 'h' Null Null) (Node 'j' Null Null))
test5 :: BinarySearchTree Integer
test5 = Node 92 (Node 11 Null Null) (Node 101 Null (Node 102 Null Null))

--newly added
treeBalance :: Ord a => BinarySearchTree a -> BinarySearchTree a
treeBalance tree = helper treeList
    where
    helper [] = Null
    helper t = Node (t !! (length t `div` 2)) (helper $ take (length t `div` 2) t) (helper $ drop ((length t `div` 2)+1) t)
    treeList = sort $ treeFlatten tree

