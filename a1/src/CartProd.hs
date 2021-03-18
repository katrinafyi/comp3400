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

-- | rewrite fmap for lists because it's banned i guess.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (a:as) = f a : map' f as

flat' :: [[a]] -> [a]
flat' (x:xs) = x ++ flat' xs
flat' [] = []

cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (l:ls) =
    flat' $ map' (\x -> map' (x:) rest) l
    where rest = cartProd ls
-- (this was very hard.)
-- also just sequence, apparently.

