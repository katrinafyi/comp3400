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

import           Data.Function (on)
import           Data.Monoid (Sum(Sum, getSum))

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
data Pair a b = Pair { pairFst :: a, pairSnd :: b } deriving Show

instance Eq a => Eq (Pair a b) where
  (==) = (==) `on` pairFst

instance Ord a => Ord (Pair a b) where
  compare = compare `on` pairFst

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair a1 b1 <> Pair a2 b2 = Pair (a1 <> a2) (b1 <> b2)

type SumList a = Pair (Sum a) [a]

singletonSList :: a -> SumList a
singletonSList x = Pair (Sum x) [x]

-- | Stores two maximum paths for the current tree node.
-- The top path is the maximum path which starts from the current top node,
-- and the max path is the maximum path over all paths in the subtrees.
data MaxPath a = MaxPath { getTopPath :: SumList a, getMaxPath :: SumList a }
  deriving Show

-- | Creates a MaxPath from a single node.
singletonPath :: a -> MaxPath a
singletonPath x = MaxPath sl sl
  where sl = singletonSList x

-- | Joins the given parent element and children paths.
-- Paths are joined with node values by the Semigroup <> and maximum is taken
-- over the Ord instance.
joinPaths :: (Ord a, Num a) => a -> MaxPath a -> MaxPath a -> MaxPath a
joinPaths x (MaxPath t1 m1) (MaxPath t2 m2) = MaxPath t' m'
  where
    x' = singletonSList x
    -- top path must include x, then possibly a top path from its children.
    t' = x' `max` (x' <> max t1 t2)
    -- max path could be the top path, a path going through x and both child
    -- top paths, or just some child's max path without x.
    m' = t' `max` (t1 <> x' <> t2) `max` m1 `max` m2

foldMaxPath :: (Ord a, Num a) => TreeF a (MaxPath a) -> MaxPath a
foldMaxPath (LeafF x) = singletonPath x
foldMaxPath (NodeF l x r) = joinPaths x l r

maxPath' :: (Ord a, Num a) => Tree a -> MaxPath a
maxPath' = cataTree foldMaxPath

maxPath :: Tree Int -> Int
maxPath = getSum . pairFst . getMaxPath . maxPath'
