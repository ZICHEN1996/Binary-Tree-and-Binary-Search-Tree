module BinaryTree (
    BinaryTree (Null, Node, element, left, right),
    treeSize,      -- :: (Integral b) => BinaryTree a -> b
    treeDepth,     -- :: (Integral b) => BinaryTree a -> b
    treeFlatten ,  -- :: BinaryTree a -> [a]
    treeLeaves,    -- :: BinaryTree a -> [a]
    treeMap        -- :: (a->b) -> BinaryTree a - > BinaryTree b
) where

data List a
  = Empty
  | Attach a
           (List a)
data BinaryTree a
  = Null
  | Node { element :: a
        ,  left, right :: BinaryTree a}
  deriving (Show, Eq)
-- Exercise 1
treeSize
  :: (Integral b)
  => BinaryTree a -> b
treeSize Null = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right
treeDepth
  :: Integral b
  => BinaryTree a -> b
treeDepth tree = helper tree 0
    where
        helper Null n = n
        helper (Node _ left right) n = max (helper left (n+1)) (helper right (n+1))
treeFlatten :: BinaryTree a -> [a]
treeFlatten Null = []
treeFlatten (Node n left right) = treeFlatten left ++ [n] ++ treeFlatten right
treeLeaves :: BinaryTree a -> [a]
treeLeaves Null = []
treeLeaves (Node a Null Null) = [a]
treeLeaves (Node a left right) = treeLeaves left ++ treeLeaves right
-- Exercise 2
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Null = Null
treeMap f (Node n t1 t2) = Node (f n) (treeMap f t1) (treeMap f t2)
test1, test2 :: BinaryTree Float
test1 = Node 0.0 Null Null
test2 = Node 10.9 Null (Node (-1.0) Null Null)
test3 :: BinaryTree String
test3 = Node "Galvanic" (Node "metal" (Node "beats" Null Null)
    (Node "stomp" Null Null)) (Node "out" Null (Node "louder" Null
        Null))