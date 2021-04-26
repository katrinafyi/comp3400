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
    > treeB = Node (Leaf 9) (-10) (Node (Lead 15) 20 (Leaf 7))

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

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

foldTree :: (a -> b -> b -> b) -> (a -> b) -> Tree a -> b
foldTree _ g (Leaf x) = g x
foldTree f g (Node l x r) = f x l' r'
  where
    l' = foldTree f g l
    r' = foldTree f g r

-- | Stores two maximum paths for the current tree node.
-- The top path is the maximum path which starts from the current top node,
-- and the max path is the maximum path over all paths in the subtrees.
data MaxPath a = MaxPath { getTopPath :: a, getMaxPath :: a }
  deriving Show

-- | Creates a MaxPath from a single node.
singletonPath :: a -> MaxPath a
singletonPath x = MaxPath x x

-- | Joins the given parent element and children paths.
-- Paths are joined with node values by the Semigroup <> and maximum is taken
-- over the Ord instance.
joinPaths :: (Ord a, Semigroup a) => a -> MaxPath a -> MaxPath a -> MaxPath a
joinPaths x (MaxPath t1 x1) (MaxPath t2 x2) = MaxPath t' x'
  where
    -- top path must include x, then the larger top path from its children.
    t' = x <> max t1 t2
    -- max path could be the top path, a path going through x and both child
    -- top paths, or just some child's max path without x.
    x' = t' `max` (x <> t1 <> t2) `max` x1 `max`x2

maxPath' :: (Ord a, Semigroup a) => Tree a -> MaxPath a
maxPath' = foldTree joinPaths singletonPath

maxPath :: Tree Int -> Int
maxPath = getSum . getMaxPath . maxPath' . fmap Sum
