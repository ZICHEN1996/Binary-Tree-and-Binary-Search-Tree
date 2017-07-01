module BinaryTreeTests where

import BinaryTree
import BinaryTreeTestGenerator
import Test.QuickCheck

prop_unflattened_size :: BinaryTree String -> Bool
prop_unflattened_size tree = length (treeFlatten tree) == treeSize tree

prop_keep_size :: BinaryTree Integer -> Bool
prop_keep_size tree =
  (treeSize tree :: Integer) == treeSize (treeMap (+ 1) tree)

prop_reverse_function :: BinaryTree Integer -> Bool
prop_reverse_function tree = tree == treeMap (+ (-1)) (treeMap (+ 1) tree)

prop_larger_than_deep :: BinaryTree Integer -> Bool
prop_larger_than_deep tree = (treeSize tree :: Integer) >= treeDepth tree

--new property
prop_size_more_than_leaves :: BinaryTree Integer -> Bool
prop_size_more_than_leaves tree = (treeSize tree) >= length (treeLeaves tree)

-- QuickCheck tests to run
test_prop_unflattened_size :: IO ()
test_prop_unflattened_size = quickCheck prop_unflattened_size

test_prop_keep_size :: IO ()
test_prop_keep_size = quickCheck prop_keep_size

test_prop_reverse_function :: IO ()
test_prop_reverse_function = quickCheck prop_reverse_function

test_prop_larger_than_deep :: IO ()
test_prop_larger_than_deep = quickCheck prop_larger_than_deep

--new test
test_prop_size_more_than_leaves :: IO()
test_prop_size_more_than_leaves = quickCheck prop_size_more_than_leaves
