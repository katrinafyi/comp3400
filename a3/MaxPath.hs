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
    PathResult :: Tree a -> a
that returns
    maximum [ sum path | path in (All paths of tree)] -- Note:  not Haskell code

=======
EXAMPLE
=======

> PathResult treeA
6                   -- 1 + 2 + 3

> PathResult treeB
42                  -- 15 + 20 + 7

NOTES:  Your code doesn't have to be the BEST implementation, but it shouldn't
be the worst one either.  If you implement a greedy algorithm (the one which
enumerates all paths FIRST to then find the one with largest sum) your code will
likely use too much memory and crash the grader.

You should run your code on some larger examples.

--}

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
    PathResult :: Tree a -> a
that returns
    maximum [ sum path | path in (All paths of tree)] -- Note:  not Haskell code

=======
EXAMPLE
=======

> PathResult treeA
6                   -- 1 + 2 + 3

> PathResult treeB
42                  -- 15 + 20 + 7

NOTES:  Your code doesn't have to be the BEST implementation, but it shouldn't
be the worst one either.  If you implement a greedy algorithm (the one which
enumerates all paths FIRST to then find the one with largest sum) your code will
likely use too much memory and crash the grader.

You should run your code on some larger examples.

--}

import           Data.Monoid (Sum(Sum, getSum))
import           Data.Function (on)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
  deriving Show

data MonoidList m a = MonoidList { monoidAcc :: m a, getMonoidList :: [m a] }
  deriving Show

instance Eq (m a) => Eq (MonoidList m a) where
  (==) = (==) `on` monoidAcc

instance Ord (m a) => Ord (MonoidList m a) where
  compare = compare `on` monoidAcc

type SumList a = MonoidList Sum a

(<>:) :: (Monoid (m a)) => m a -> MonoidList m a -> MonoidList m a
x <>: (MonoidList s xs) = MonoidList (x <> s) (x:xs)

(+:) :: Num a => a -> SumList a -> SumList a
(+:) = (<>:) . Sum

singletonSumList :: a -> SumList a
singletonSumList x = MonoidList (Sum x) [Sum x]

data MaxPath a = MaxPath { fromRoot :: SumList a, fromAny :: SumList a }
  deriving Show

emptyPath :: Num a => MaxPath a
emptyPath = MaxPath x x
  where x = MonoidList mempty []

makePath :: Ord a => SumList a -> SumList a -> MaxPath a
makePath r a = MaxPath r (max r a)

addParent :: (Ord a, Num a) => a -> MaxPath a -> MaxPath a
addParent x (MaxPath r a) = makePath (x +: r) a

mergePaths :: Ord a => MaxPath a -> MaxPath a -> MaxPath a
mergePaths (MaxPath r1 a1) (MaxPath r2 a2) = makePath (max r1 r2) (max a1 a2)

maxPath' :: Tree Int -> MaxPath Int
maxPath' (Leaf x) = addParent x $ emptyPath
maxPath' (Node l x r) = addParent x $ mergePaths left right
  where
    left = maxPath' l
    right = maxPath' r

maxPath :: Tree Int -> Int
maxPath = getSum . monoidAcc . fromAny . maxPath'
