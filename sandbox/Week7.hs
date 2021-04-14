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

data a |*| b = Product a b deriving (Show)
data a |+| b = Sum1 a | Sum2 b deriving (Show)

infix 9 |*|
infix 8 |+|

ex3 :: a |*| (b |+| c) -> (a |*| b) |+| (a |*| c)
ex3 (Product a (Sum1 b)) = Sum1 (Product a b)
ex3 (Product a (Sum2 c)) = Sum2 (Product a c)

ex4 :: (a |*| b) |+| (a |*| c) -> a |*| (b |+| c)
ex4 (Sum1 (Product a b)) = Product a (Sum1 b)
ex4 (Sum2 (Product a c)) = Product a (Sum2 c)