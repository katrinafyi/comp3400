{-# LANGUAGE ScopedTypeVariables #-}
module Written where

import Data.List (genericLength)
import Test.QuickCheck

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}

-- | 1.1. Recursively computes the average of the given list.
p_average :: (Fractional a) => [a] -> a
p_average [] = 0
p_average (x:xs) = (x + (n-1) * p_average xs) / n
  where
    n = 1 + genericLength xs

-- | 1.2. Uses tail recursion to compute average of the given list of numbers.
-- Arguments are running length, running sum, remaining numbers.
-- Returns average or NaN if list is empty.
h_average :: (Fractional a) => a -> a -> [a] -> a
h_average n s [] = s / n
h_average n s (x:xs) = h_average (n+1) (s+x) xs

-- | 1.3.
average :: (Fractional a) => [a] -> a
average = h_average 0 0


-- | 1.4. Iteration invariant.
{-

The invariant is:

  h_average n s xs == (sum xs + s) / (length xs + n).

Assuming this holds, we have

  average xs = h_average 0 0 xs
             = (sum xs + 0) / (length xs + 0)
             = sum xs / length xs

which is the definition of the average, proving correctness of the average
function.

-}

-- | 1.5. Prove h_average.
{-

We will prove this via induction. For the base case where xs = [],

  RHS = (sum [] + s) / (length [] + n)  -- by definition of RHS
      = (0 + s) / (0 + n)               -- sum and length of [] are 0
      = s / n                           -- maths
      = h_average n s []                -- h_average base case
      = LHS                             -- by definition of LHS

Assume

  h_average n s xs == (sum xs + s) / (length xs + n)

holds for some list xs of length >= 0. We will prove it holds for (x:xs) of
length one greater.

  LHS = h_average n s (x:xs)                    -- by definition of LHS
      = h_average (n+1) (s+x) xs                -- h_average recursive case
      = (sum xs + (s+x)) / (length xs + (n+1))  -- inductive hypothesis
      = (sum (x:xs) + s) / (length (x:xs) + n)  -- manipulating sum and length
      = RHS                                     -- as required

We have shown the iteration invariant holds for the empty list and if it holds
for xs, then it holds for (x:xs). By induction on the list, it holds for lists
of all lengths.

-}

-- | 1.6. Property tests.

-- For non-empty lists, multiplying the average by the length is the sum.
prop_average1 :: [Float] -> Property
prop_average1 =
  \(xs :: [Float]) -> not (null xs) ==> genericLength xs * average xs == sum xs

-- For non-empty lists, average is between the minimum and maximum numbers.
prop_average2 :: [Float] -> Property
prop_average2 =
  \(xs :: [Float]) -> not (null xs) ==> minimum xs <= average xs && average xs <= maximum xs



type State = Int
newtype ST a = S (State -> (a,State))

apply :: ST a -> State -> (a, State)
apply (S st) x = st x


-- | 2. Functor.
instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    a <- st
    pure $ g a

-- | 2. Applicative.
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    a <- stx
    pure $ f a

instance Monad ST where
  --  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = apply st s in apply (f x) s')


{-
zip1 :: (a -> b) -> [a] -> [b]
> zip1 succ [1,2,3]
[2,3,4]

zip2 :: (a -> b -> c) -> [a] -> [b] -> [c]
> zip2 (+) [1,2,3] [4,5,6]
[5,7,9]

zip3 :: (a -> b -> c) -> [a] -> [b] -> [c] -> [d]
> zip3 (\x y z -> x + y + z) [1,2,3] [4,5,6] [7,8,9]
[12, 15, 18]

zip2
> zip2 (+) [1] [1,2,3]
[2]
> zip2 (+) [1,2,3] [1]
[2]
-}

-- Note: We define a newtype synonym so the code compiles with our custom
-- Functor and Applicative instances. ZL is short for ZipList.
newtype ZL a = ZL [a] deriving (Show, Eq)


-- | 3. List with zipping applicative.

-- | 3.1.
{-
instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x:xs) = g x : fmap g xs
  -- identical to ordinary lists.

-- | 3.2.
instance Applicative [] where
  -- pure :: a -> [a]
  pure x = repeat x

  -- (<*>) :: [a -> b] -> [a] -> [b]
  (f:fs) <*> (x:xs) = f x : (fs <*> xs)
  _ <*> _ = []
-}

-- | 3.3. Prove: x <*> pure y = pure (\g -> g y) <*> x.
{-
We want to prove the statement:

  x <*> pure y = pure (\g -> g y) <*> x.

First, note the types:

  x :: [a -> b]
  y :: a
  pure y :: [a]
  pure (\g -> g y) :: [(a -> b) -> b]

We will prove with induction on the x list, starting with x = [].
For this case, we have

  LHS = [] <*> pure y                        -- substituting [] into LHS
      = [] <*> repeat y                      -- apply pure
      = []                                   -- apply (<*>)
      = repeat (\g -> g y) <*> []            -- unapply (<*>)
      = pure (\g -> g y) <*> []              -- unapply pure
      = RHS                                  -- where x = []

Now assume

  x <*> pure y = pure (\g -> g y) <*> x

holds for x = zs and we will prove for x = z:zs.

  LHS = (z:zs) <*> pure y                  -- definition of x
      = (z:zs) <*> repeat y                -- apply pure
      = z y : (zs <*> repeat y)            -- apply one step of <*>
      = z y : (zs <*> pure y)              -- unapply pure
      = z y : (pure (\g -> g y) <*> zs)    -- inductive hypothesis
      = z y : (repeat (\g -> g y) <*> zs)  -- apply pure
      = (\g -> g y) z : (repeat (\g -> g y) <*> zs)
                                           -- manipulation of (z y)
      = repeat (\g -> g y) <*> (z:zs)      -- unapply one step of <*>
      = pure (\g -> g y) <*> (z:zs)        -- unapply pure
      = RHS                                -- where x = z:zs

We have shown the statement holds for the empty list and if it holds
for zs, then it holds for (z:zs). By induction on the list, it holds for lists
of all lengths.
-}
