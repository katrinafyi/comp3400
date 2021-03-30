module Blockus (tile) where

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

type Board a = [[a]]

hstack :: Board a -> Board a -> Board a
hstack = zipWith (++)

vstack :: Board a -> Board a -> Board a
vstack = (++)

-- toList :: Four a -> [[a]]
-- toList (Leaf a) = [[a]]
-- toList (Four a b c d) = vstack (hstack (toList a) (toList b)) (hstack (toList c) (toList d))

transpose :: Board a -> Board a
transpose [] = []
transpose [[]] = [[]]
transpose (x:xs) = (corner ++ top) : zipWith (:) left rest
  where corner = (take 1 . take 1) x
        top = concatMap (take 1) xs
        left = drop 1 x
        rest = transpose $ drop 1 <$> xs

rotateCW :: Board a -> Board a
rotateCW = transpose . reverse

rotateCCW :: Board a -> Board a
rotateCCW = reverse . transpose

mapB :: (a -> b) -> Board a -> Board b
mapB = fmap . fmap

foldlB :: (b -> a -> b) -> b -> Board a -> b
foldlB = foldl . foldl

maxB :: Board Int -> Int
maxB = foldlB max 0

setCorner :: a -> Board a -> Board a
setCorner x b = case b of
  ((_:tops):rest) -> (x:tops) : rest
  other -> other

numTiles :: Integral n => n -> n
numTiles 0 = 0
numTiles n = 4 * numTiles (n-1) + 1

seqTile :: Int -> Board Int -> Board Int
seqTile n = mapB (+ offset)
  where offset = numTiles n

tile :: Int -> Board Int
tile 0 = [[0]]
tile n = vstack (hstack topLeft topRight) (hstack bottomLeft bottomRight)
  where (t0:t1:t2:t3:_) = iterate (seqTile (n-1)) (tile (n-1))
        centre = numTiles n
        topLeft = t0
        topRight = rotateCCW $ setCorner centre t1
        bottomRight = setCorner centre t2
        bottomLeft = rotateCW $ setCorner centre t3