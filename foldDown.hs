module FoldDown () where

import Prelude hiding (foldr)
import qualified Prelude as P

x :: [[Int]]
x = [[1, 2, 3],
    [4, 5, 6]]

-- [[1,4], [2,5], [3,6]]
-- transpose :: [[a]] -> [[a]]


-- [1, 2, 3]
-- (:)(:)(:)
-- [4, 5, 6]
-- (:)(:)(:)
-- [ [], [], [], ]

-- foldDown :: (a -> b -> b) -> b -> [[a]] -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b
--

foldDown :: (a -> b -> b) -> b -> [[a]] -> [b]
foldDown _ b [] = repeat b
foldDown f b (a:as) = zipWith f a rest
    where rest = foldDown f b as

transpose :: [[a]] -> [[a]]
transpose = foldDown (:) []
-- a :: [a]
-- as :: [[a]]

{-

(:) :: a -> [a] -> [a]
f   :: a ->  b  ->  b

a = [1, 2, 3] :: [a]
rest = [ [4, 7], [5, 8], [6, 9] ]

as = [ [4, 5, 6], [7, 8, 9] ]

-}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b [] = b
foldr f b (a:as) = f a rest
    where rest = foldr f b as

foldDown' :: (a -> b -> b) -> b -> [[a]] -> [b]
foldDown' _ b [] = repeat b
foldDown' f b (a:as) = (zipWith f) a rest
    where rest = foldDown' f b as

f1 :: Foldable f => (a -> b -> b) -> b -> f [a] -> [b]
f1 f b = P.foldr (zipWith f) (repeat b)

-- we're doing something with the current value "a".
-- and combining it, via f, with the result of foldDown the "as".
y = [1, 2] : x
