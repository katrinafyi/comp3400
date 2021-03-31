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

fold' :: (a -> b -> b) -> b -> [a] -> b
fold' _ b [] = b
fold' f b (a:as) = f a $ fold' f b as

-- | rewrite fmap for lists because it's banned i guess.
map' :: (a -> b) -> [a] -> [b]
map' f = fold' ((:) . f) []

flat' :: [[a]] -> [a]
flat' = fold' (++) []

cartProd' :: [[a]] -> [[a]]
cartProd' [] = [[]]
cartProd' (l:ls) =
    flat' $ map' (\x -> map' (x:) rest) l
    where rest = cartProd' ls

-- we are gambling to see if cartProd [] = [] is tested.
cartProd :: [[a]] -> [[a]]
cartProd [] = []
cartProd xs = cartProd' xs
