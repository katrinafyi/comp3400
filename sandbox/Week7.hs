{-# LANGUAGE TypeOperators #-}

module Week7 where

import Data.Functor.Contravariant
import Data.Coerce

import qualified Control.Category as C

ex1 :: (a -> b -> c) -> b -> a -> c
ex1 f b a = f a b


ex2 :: (a -> b) -> ((a -> c) -> c) -> ((b -> c) -> c)
ex2 a2b acc = \b2c -> acc (b2c . a2b)

ex2' :: (a -> b) -> Op c (Op c a) -> Op c (Op c b)
ex2' = contramap . contramap

ex2''' :: Op b a -> Op c (Op c a) -> Op c (Op c b)
ex2''' = undefined


f2 :: Contravariant f => (a -> b) -> f (f a) -> f (f b)
f2 = contramap . contramap

-- ex2'' :: Op b a -> Op c (Op c a) -> Op c (Op c b)
ex2'' :: (a -> b) -> ((a -> c) -> c) -> ((b -> c) -> c)
ex2'' a b = getOp $ contramap Op $ f2 a (Op getOp C.. Op b)

data a |*| b = Prod a b deriving (Show)
data a |+| b = Sum1 a | Sum2 b deriving (Show)

infixl 8 |*|
infixl 7 |+|

ex3 :: a |*| (b |+| c) -> (a |*| b) |+| (a |*| c)
ex3 (Prod a (Sum1 b)) = Sum1 (Prod a b)
ex3 (Prod a (Sum2 c)) = Sum2 (Prod a c)

ex4 :: (a |*| b) |+| (a |*| c) -> a |*| (b |+| c)
ex4 (Sum1 (Prod a b)) = Prod a (Sum1 b)
ex4 (Sum2 (Prod a c)) = Prod a (Sum2 c)

-- x ^ (a + b) = (x ^ a) ^ b

{-
What is a^b in types? With numbers, this is a * ... * a, b times.
Consider:
Ordering = LT | EQ | GT
Bool = True | False

How many functions are there Ordering -> Bool? For each of the 3 orderings,
we have two possible values. Therefore, this is just 2 * 2 * 2 = 2^3.

Observe that this is |Bool| ^ |Ordering|.

arithmetic      types       logic
---------------------------------
+               Either      ||
*               (,)         &&
^               (->)        ==>
1               ()          True
0               void        False (_|_)
-}

data a |^| b = Pow { getPow :: (b -> a) }

ex5 :: c |^| (a |*| b) -> (c |^| a) |^| b
ex5 (Pow f) = Pow (\b -> Pow (\a -> f (Prod a b)))

ex5' :: ((a, b) -> c) -> (b -> a -> c)
ex5' = flip . curry
-- of course, we have an isomorphism from (a,b) to (b,a). this is swap.

ex6 :: x |^| a |*| x |^| b -> x |^| (a |+| b)
ex6 (Prod (Pow ax) (Pow bx)) = Pow $
    \ab -> case ab of
        Sum1 a -> ax a
        Sum2 b -> bx b
