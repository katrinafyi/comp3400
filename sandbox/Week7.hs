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

type a |*| b = (a, b)
type a |+| b = Either a b

infixl 8 |*|
infixl 7 |+|

ex3 :: a |*| (b |+| c) -> (a |*| b) |+| (a |*| c)
ex3 (a, Left b) = Left (a, b)
ex3 (a, Right c) = Right (a, c)

ex4 :: (a |*| b) |+| (a |*| c) -> a |*| (b |+| c)
ex4 (Left (a, b)) = (a, (Left b))
ex4 (Right (a, c)) = (a, (Right c))

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

type a |^| b = b -> a

ex5 :: c |^| (a |*| b) -> (c |^| a) |^| b
ex5 f = \b a -> f (a, b)

ex5' :: ((a, b) -> c) -> (b -> a -> c)
ex5' = flip . curry
-- of course, we have an isomorphism from (a,b) to (b,a). this is swap.

ex6 :: x |^| a |*| x |^| b -> x |^| (a |+| b)
ex6 (ax, bx) = \ab -> case ab of
        Left a -> ax a
        Right b -> bx b

data NotList a = NotList (() |+| a |*| NotList a)
-- L = 1 + a L
-- d/da (1 + a + a^2 + ...) = 1 + 2a + 3a^2 + 4a^3 + ...

toList :: NotList a -> [a]
toList (NotList (Left ())) = []
toList (NotList (Right (a, x))) = a : toList x

-- [a] ~= x^i

{-
x ^ 0 = 1   Void -> x = absurd
x ^ 1 = x   () -> x
x + 0 = x
x * 1 = x
-}

