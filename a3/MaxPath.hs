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

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

maxPath :: Tree Int -> Int
maxPath = undefined