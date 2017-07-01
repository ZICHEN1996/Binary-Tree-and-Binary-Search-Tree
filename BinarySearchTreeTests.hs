module BinarySearchTreeTests where

import BinarySearchTree
import BinarySearchTreeTestGenerator
import BinaryTree
import Data.List
import Test.QuickCheck

prop_unflattened_size :: BinarySearchTree String -> Bool
prop_unflattened_size tree = length (treeFlattenOrdered tree) == treeSize tree

prop_min_element :: BinarySearchTree Integer -> Bool
prop_min_element tree =
  tree == Null || treeMinimum tree == head (treeFlattenOrdered tree)

prop_add_one_element :: BinarySearchTree Char -> Bool
prop_add_one_element tree
  | treeContains 'a' tree =
    (treeSize tree :: Integer) == treeSize (treeInsert 'a' tree)
  | otherwise = 1 + (treeSize tree :: Integer) == treeSize (treeInsert 'a' tree)

prop_add_min_max_elements :: BinarySearchTree Char -> Bool
prop_add_min_max_elements tree =
  tree == Null ||
  tree == treeInsert (treeMaximum tree) (treeInsert (treeMinimum tree) tree)

prop_distinct_elements :: String -> Bool
prop_distinct_elements list =
  length (nub list) ==
  length (treeFlattenOrdered (insert_list_to_tree list Null))
  where
    insert_list_to_tree list' tree =
      case list' of
        [] -> tree
        x:xs -> treeInsert x (insert_list_to_tree xs tree)

prop_is_valid_binary_search_tree :: BinarySearchTree Char -> Bool
prop_is_valid_binary_search_tree tree = check_tree_bounds tree Nothing Nothing
  where
    check_tree_bounds
      :: (Ord a)
      => BinarySearchTree a -> Maybe a -> Maybe a -> Bool
    check_tree_bounds c_tree min_e max_e =
      case c_tree of
        Null -> True
        Node e left right ->
          min_e `maybe_less_than` Just e &&
          Just e `maybe_less_than` max_e &&
          check_tree_bounds left min_e (maybe_minmax min (Just e) max_e) &&
          check_tree_bounds right (maybe_minmax max min_e (Just e)) max_e
    maybe_minmax :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
    maybe_minmax f a b =
      case (a, b) of
        (Nothing, Nothing) -> Nothing
        (_, Nothing) -> a
        (Nothing, _) -> b
        (Just a_v, Just b_v) -> Just (f a_v b_v)
    maybe_less_than
      :: (Ord a)
      => Maybe a -> Maybe a -> Bool
    maybe_less_than a b =
      case (a, b) of
        (Nothing, Nothing) -> True
        (_, Nothing) -> True
        (Nothing, _) -> True
        (Just a_v, Just b_v) -> a_v < b_v

prop_reasonably_balanced :: BinarySearchTree Integer -> Bool
prop_reasonably_balanced t =
  tree == Null ||
  ((treeSize tree :: Integer) < 4) ||
  (fromIntegral (treeDepth tree :: Integer) :: Double) <=
  2 * logBase 2 (1 + fromIntegral (treeSize tree :: Integer))
    where tree = treeBalance t

test_prop_unflattened_size :: IO ()
test_prop_unflattened_size = quickCheck prop_unflattened_size

test_prop_min_element :: IO ()
test_prop_min_element = quickCheck prop_min_element

test_prop_add_one_element :: IO ()
test_prop_add_one_element = quickCheck prop_add_one_element

test_prop_add_min_max_elements :: IO ()
test_prop_add_min_max_elements = quickCheck prop_add_min_max_elements

test_prop_distinct_elements :: IO ()
test_prop_distinct_elements = quickCheck prop_distinct_elements

test_prop_is_valid_binary_search_tree :: IO ()
test_prop_is_valid_binary_search_tree =
  quickCheck prop_is_valid_binary_search_tree

test_prop_reasonably_balanced :: IO ()
test_prop_reasonably_balanced = quickCheck prop_reasonably_balanced

--new tests
prop_max_element :: BinarySearchTree Integer -> Bool
prop_max_element tree = tree == Null || (treeMaximum tree) == (last $ treeFlatten tree)

test_prop_max_element :: IO()
test_prop_max_element = quickCheck prop_max_element

prop_tree_flatten_order :: BinarySearchTree Integer -> Bool
prop_tree_flatten_order tree = treeFlattenOrdered tree == (sort $ brTreeFlatten tree)
    where
    brTreeFlatten Null = []
    brTreeFlatten (Node n t1 t2) = [n] ++ brTreeFlatten t1 ++ brTreeFlatten t2

test_prop_tree_flatten_order :: IO()
test_prop_tree_flatten_order = quickCheck prop_tree_flatten_order