module CartProd (cartProd) where

{-
*DO NOT* use
  map, zip, foldl, or foldr
to solve this question

*DO NOT* load any modules.

Given a list of lists [xs0, xs2, ..., xsk], return the CARTESIAN PRODUCT of
those lists.

=======
EXAMPLE
=======
> cartProd [[1, 2], [3], [4, 5]]
[[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]]
-}

-- import Test.QuickCheck

fold' :: (a -> b -> b) -> b -> [a] -> b
fold' _ b [] = b
fold' f b (a:as) = f a $ fold' f b as

map' :: (a -> b) -> [a] -> [b]
map' f = fold' ((:) . f) []

flat' :: [[a]] -> [a]
flat' = fold' (++) []

cartProd :: [[a]] -> [[a]]
cartProd = fold' go [[]]
  where
    go :: [a] -> [[a]] -> [[a]]
    go [] _ = []
    go _ [] = []
    go l rest = l >>= \x -> map' (x:) rest

-- prop_CartProd :: [[Int]] -> Property
-- prop_CartProd xs = (product $ fmap length xs) <= 100 ==> cartProd' xs == sequence xs

-- main :: IO ()
-- main = do
--   quickCheck (noShrinking prop_CartProd)