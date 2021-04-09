module Blockus2 (tile) where

import Data.Functor.Classes (Show1, liftShowsPrec, showsPrec1)

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

-- | Homogeneous tuple of exactly four elements.
data Four a = Four a a a a

instance Show1 Four where
  liftShowsPrec showP showL p fa = showParen (p > 10)
    $ showString "Four" . foldr (\a b -> showChar ' ' . showP 11 a . b) id fa

instance (Show a) => Show (Four a) where showsPrec = showsPrec1

instance Functor Four where
  fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Applicative Four where
  pure a = Four a a a a
  Four fa fb fc fd <*> Four a b c d = Four (fa a) (fb b) (fc c) (fd d)

instance Foldable Four where
  foldr f x (Four a b c d) = f a $ f b $ f c $ f d x

instance Traversable Four where
  -- sequenceA :: Applicative f => Four (f a) -> f (Four a)
  sequenceA (Four a b c d) = Four <$> a <*> b <*> c <*> d


-- Free monad definition and typeclass definitions.

data Free f a = Pure a | Free (f (Free f a))

instance (Show a, Show1 f) => Show (Free f a) where
  showsPrec p (Pure x) = showParen (p > 10)
    $ showString "Pure " . showsPrec 11 x
  showsPrec p (Free fx) = showParen (p > 10)
    $ showString "Free " . liftShowsPrec showsPrec showList (11) fx

-- instance (Show1 f, Show a) => Show (Free f a) where
--   showsPrec = showsPrec1

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap f <$> x

instance Functor f => Applicative (Free f) where
  pure = Pure
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

-- better expressed as:
-- mapFree :: (Functor f, Functor g) => (forall b. f b -> g b) -> Free f a -> Free g a
-- but that would require RankNTypes.
mapFree :: (Functor f, Functor g) => (f (Free g a) -> g (Free g a)) -> Free f a -> Free g a
mapFree f = foldFree (Free . f) Pure

foldFree :: (Functor f) => (f b -> b) -> (a -> b) -> Free f a -> b
foldFree f b (Pure x) = b x
foldFree f b (Free x) = f $ foldFree f b <$> x

-- Functions to transform Four and the Board.

rotateCCW :: Four a -> Four a
rotateCCW (Four a b c d) = Four b c d a

rotateCW :: Four a -> Four a
rotateCW (Four a b c d) = Four d a b c

setCorner :: a -> Free Four a -> Free Four a
setCorner x (Pure _) = Pure x
setCorner x (Free (Four a b c d)) = Free $ Four a' b c d
  where a' = setCorner x a
-- can this be a lens?


-- Tiles the board.

type Board = Free Four Int

numTiles :: Int -> Int
numTiles n = (4^n - 1) `div` 3

tileBoard :: Int -> Board
tileBoard 0 = Pure 0
tileBoard n = Free $ Four a b c d
  where
    -- tiling of top left is just a tiling of a smaller tile.
    a = tileBoard (n-1)
    -- maximum number in the tiling "a".
    m = numTiles (n-1)

    -- generate tiles for the other 3 quadrants by incrementing and rotating.
    b = mapFree rotateCCW $ setNum $ fmap (+1*m) a
    c = setNum $ fmap (+2*m) a
    d = mapFree rotateCW $ setNum $ fmap (+3*m) a

    setNum :: Board -> Board
    setNum = setCorner (4*m + 1)

-- Functions for converting a Board to a list of lists.

hstack :: [[a]] -> [[a]] -> [[a]]
hstack = zipWith (++)

vstack :: [[a]] -> [[a]] -> [[a]]
vstack = (++)

fourToList :: Four [[a]] -> [[a]]
fourToList (Four a b c d) = vstack (hstack a b) (hstack d c)

boardToList :: Free Four a -> [[a]]
boardToList = foldFree fourToList (pure . pure)

leftPad :: Int -> a -> [a] -> [a]
leftPad n a xs = replicate (n - length xs') a ++ xs
    where xs' = take n xs

padSpace :: String -> String
padSpace = leftPad 3 ' '

boardToString :: Show a => Free Four a -> String
boardToString = unlines . fmap concat . boardToList . fmap (padSpace . show)

showBoard :: Show a => Free Four a -> IO ()
showBoard = putStr . boardToString

-- t = tileBoard 2

-- Final function.

tile :: Int -> [[Int]]
tile n
  | n < 0 = []
  | otherwise = boardToList $ tileBoard n

