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

cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (l:ls) = case l of
    [] -> []
    (x:xs) -> ((x:) <$> cartProd ls) ++ cartProd (xs:ls)
-- (this was very hard.)


-- this is complex. we are recursing into the first list and across the list
-- of lists.
--
-- as a brief explanation of the base cases, cartProd [] needs to be [[]]
-- because this base case is reached from the cartProd ls recursion. if this
-- was just [], then the <$> it is applied to would always return [] and the
-- result would always be empty. this is the end of the recursion across the
-- list of lists ls, if that makes sense.
--
-- cartProd [[]] needs to return [] because this is reached from the call
-- cartProd (xs:ls) with xs = []. this represents the end of the recursion into
-- the leftmost list l. this needs to be [] because it will be ++'d to build
-- the result. if it was, say, [] -> cartProd ls, then the result would include
-- _shorter_ products which is not what we want.
--
-- intuitively, this also makes sense because cartProd [..., [], ...] should
-- return an empty list. this is just the base case of that.
--
-- the recursive case is much more simple. stepping through the example of
-- cartProd [[1, 2], [3], [4, 5]], this gives us
-- l = [1,2]
-- ls = [[3], [4, 5]]
-- and
-- x = 1
-- xs = [2]
-- the expression left of ++ prepends x to the cartesian product of remaining
-- ls. the right expression handles the cartesian product of the rest of the
-- xs recursively in the same way.
