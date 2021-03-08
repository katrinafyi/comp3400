module ApplyAll (applyAll) where

{-
*DO NOT* load any modules.

Given a list of functions [f0, f1, .., fn] :: [a -> b]
and a list of inputs [a0, a1, ..., am] :: [a]
return
[f0 a0, f1 a1, ... , fk ak] :: [b]
where k = min(n, m)

Your function should be TOTAL --- that is, defined for all possible inputs.

=======
EXAMPLE
=======
> applyAll [negate, (^3), succ] [1, 2, 3, 4]
[-1, 8, 4]
-}

applyAll :: [a -> b] -> [a] -> [b]
applyAll (f:fs) (a:as) = f a : applyAll fs as
applyAll _ _ = []

-- alternatively:
-- applyAll fs as = (uncurry ($)) <$> zip fs as
