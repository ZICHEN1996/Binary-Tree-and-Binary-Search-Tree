module RoseTree (
      RoseTree (RoseNode),
      treeSize,     -- :: Integral b => RoseTree a -> b
      treeDepth,    -- :: Integral b => RoseTree a -> b
      treeLeaves,   -- :: RoseTree a -> [a]
      treeMap       -- :: (a -> b) -> RoseTree a -> RoseTree b
) where
data RoseTree a =
  RoseNode a
           [RoseTree a]
  deriving (Show, Eq)
treeSize
  :: Integral b
  => RoseTree a -> b
treeSize (RoseNode _ []) = 1
treeSize (RoseNode _ xs) = 1 + sum (map treeSize xs)
treeDepth
  :: Integral b
  => RoseTree a -> b
treeDepth (RoseNode _ []) = 1
treeDepth (RoseNode n xs) = 1 + maximum (map treeDepth xs)
treeLeaves :: RoseTree a -> [a]
treeLeaves (RoseNode n []) = [n]
treeLeaves (RoseNode n xs) = (concatMap treeLeaves xs) ++ [n]
treeMap :: (a -> b) -> RoseTree a -> RoseTree b
treeMap f (RoseNode n xs) = RoseNode (f n) (map (\x -> treeMap f x) xs)
test :: RoseTree Integer
test = RoseNode 1 [RoseNode 2 [], RoseNode 3 [RoseNode 4 []],
    RoseNode 5 [RoseNode 6 [], RoseNode 7 []]]