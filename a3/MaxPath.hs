module MaxPath where


{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

A datatype for representing a BINARY TREE is
    data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

For example...

The tree binary tree given by
    (2)
   /   \
 (1)   (3)
is represented by
    > treeA = Node (Leaf 1) 2 (Leaf 3)

The tree binary tree given by
  (-10)
  /   \
(9)  (20)
     /  \
  (15)  (7)
is represented by
    > treeB = Node (Leaf 9) (-10) (Node (Leaf 15) 20 (Leaf 7))

Given a binary tree, there is a UNIQUE PATH that connects any two nodes/leaves.

For instance, in treeB:

[9, -10, 20, 15]
is THE path from (9) to (15)

[15, 20, 7]
is THE path from (15) to (7)

[-10, 20]
is THE path from (-10) -> (20)

====
TASK
====

Given
    tree :: Tree Int

Write a function
    maxPath :: Tree a -> a
that returns
    maximum [ sum path | path in (All paths of tree)] -- Note:  not Haskell code

=======
EXAMPLE
=======

> maxPath treeA
6                   -- 1 + 2 + 3

> maxPath treeB
42                  -- 15 + 20 + 7

NOTES:  Your code doesn't have to be the BEST implementation, but it shouldn't
be the worst one either.  If you implement a greedy algorithm (the one which
enumerates all paths FIRST to then find the one with largest sum) your code will
likely use too much memory and crash the grader.

You should run your code on some larger examples.

--}

import           Data.Monoid (Sum(Sum, getSum))
import           Data.Function (on)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

data TreeF a b = LeafF a | NodeF b a b
  deriving Show

instance Functor (TreeF a) where
  fmap _ (LeafF x) = LeafF x
  fmap f (NodeF l x r) = NodeF (f l) x (f r)

projectTree :: Tree a -> TreeF a (Tree a)
projectTree (Leaf x) = LeafF x
projectTree (Node l x r) = NodeF l x r

cataTree :: (TreeF a b -> b) -> Tree a -> b
cataTree f = f . fmap (cataTree f) . projectTree

-- | A list which accumulates a semigroup value along with the list.
data SList a = SList { getAcc :: a, getList :: [a] } deriving Show

instance Functor SList where
  fmap f (SList a l) = SList (f a) (fmap f l)

instance Eq a => Eq (SList a) where
  (==) = (==) `on` getAcc

instance Ord a => Ord (SList a) where
  compare = compare `on` getAcc

instance Semigroup a => Semigroup (SList a) where
  SList a1 l1 <> SList a2 l2 = SList (a1 <> a2) (l1 ++ l2)

singletonSList :: a -> SList a
singletonSList x = SList x [x]

-- | Stores two maximum paths for the current tree node.
-- The top path is the maximum path which starts from the current top node,
-- and the max path is the maximum path over all paths in the subtrees.
data MaxPath a = MaxPath { getTopPath :: SList a, getMaxPath :: SList a }
  deriving Show

instance Functor MaxPath where
  fmap f (MaxPath t m) = MaxPath (fmap f t) (fmap f m)

-- | Creates a MaxPath from a single node.
singletonPath :: a -> MaxPath a
singletonPath x = MaxPath sl sl
  where sl = singletonSList x

-- | Joins the given parent element and children paths.
-- Paths are joined with node values by the Semigroup <> and maximum is taken
-- over the Ord instance.
joinPaths :: (Ord a, Semigroup a) => a -> MaxPath a -> MaxPath a -> MaxPath a
joinPaths x (MaxPath t1 m1) (MaxPath t2 m2) = MaxPath t' m'
  where
    x' = singletonSList x
    -- top path must include x, then possibly a top path from its children.
    t' = x' `max` (x' <> max t1 t2)
    -- max path could be the top path, a path going through x and both child
    -- top paths, or just some child's max path without x.
    m' = t' `max` (t1 <> x' <> t2) `max` m1 `max` m2

foldMaxPath :: (Ord a, Semigroup a) => TreeF a (MaxPath a) -> MaxPath a
foldMaxPath (LeafF x) = singletonPath x
foldMaxPath (NodeF l x r) = joinPaths x l r

maxPath' :: (Ord a, Semigroup a) => Tree a -> MaxPath a
maxPath' = cataTree foldMaxPath

maxPath :: Tree Int -> Int
maxPath = getSum . getAcc . getMaxPath . maxPath' . fmap Sum
