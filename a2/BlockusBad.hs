module Blockus (tile) where

import Control.Applicative
import Data.Foldable (toList)
import Data.Functor.Classes (Show1, liftShowsPrec)

{-
*DO NOT* load any modules.

You may remove the comments if you like.

Recall we can cover the 2^k x 2^k Blockus board with V3 tiles provided one
corner is removed.

We can encode such a tiling with integers.  For example:

The 2 x 2 board:
0 1
1 1

The 4 x 4 board
0 2 3 3
2 2 1 3
4 1 1 5
4 4 5 5

Where "0" represents the removed corner.

Further notice the above boards can be represented by lists of rows:

The 2 x 2 board:
[[0,1],[1,1]]

The 4 x 4 board:
[[0,2,3,3],[2,2,1,3],[4,1,1,5],[4,4,5,5]]

Your task is to implement
tile :: Int -> [[Int]]
which, given k returns the covering of the 2^k x 2^k board as described.

ASSUME the removed corner is ALWAYS the north west one.  That is, if your answer
is bss then bss !! 0 !! 0 == 0.

=======
EXAMPLE
=======

NOTE your solution does not have to look identical to the following examples.
We will be conducting PROPERTY TESTING of you code.  That is, we will confirm
your board satisfies the tiling rather than comparing them with tiled boards.

> tile 1
[[0,1],[1,1]]

> tile 2
[[0,2,3,3],[2,2,1,3],[4,1,1,5],[4,4,5,5]]

-}

-- four-length tuple of identical elements
data Four a = Four a a a a deriving Show

instance Functor Four where
  fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Foldable Four where
  foldr f x (Four a b c d) = f a $ f b $ f c $ f d x

fromList :: [a] -> Four a
fromList [a,b,c,d] = Four a b c d
fromList _ = error "list does not contain exactly four elements"


data Free f a = Pure a | Free (f (Free f a))

instance (Foldable f, Show a) => Show (Free f a) where
  show (Pure x) = "Pure (" ++ show x ++ ")"
  show (Free x) = "Free (fromList " ++ (show . toList) x ++ ")"

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap f <$> x

instance Functor f => Applicative (Free f) where
  pure = Pure
  -- liftA2 :: (a -> b -> c) -> Free f a -> Free f b -> Free f c
  -- liftA2 :: (a -> (b -> c)) -> Free f a -> Free f b -> Free f c
  -- (<*>)  :: Free f (a -> b) -> Free f a -> Free f b

  Pure x <*> f = x <$> f
  Free x <*> f = Free $ (<*> f) <$> x

instance Functor f => Monad (Free f) where
  -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Free x >>= f = Free $ (>>= f) <$> x

instance (Functor f, Foldable f) => Foldable (Free f) where
  -- foldr :: (a -> b -> b) -> b -> Free f a -> b
  foldr f b (Pure x) = f x b
  foldr f b (Free x) = foldr (\fa b' -> foldr f b' fa) b x

data Corner = TopLeft | TopRight | BottomRight | BottomLeft
  deriving (Eq, Show, Ord, Enum, Bounded)

data Board = Board { boardCorner :: Corner, boardCells :: Free Four (Maybe Int)}
  deriving (Show)




tile :: Int -> [[Int]]
tile = undefined
