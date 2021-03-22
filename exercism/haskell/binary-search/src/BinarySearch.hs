module BinarySearch
    ( find
    ) where

import           Data.Array
import           Data.Maybe

length' :: Array Int a -> Int
length' = rangeSize . bounds

midpoint :: Int -> Int -> Int
midpoint a b = (a + b) `div` 2

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x
    | length' arr <= 1 = listToMaybe $ fmap fst $ filter ((== x) . snd) $ assocs
        arr
    | arr ! mid >= x = find left x
    | otherwise = find right x
  where
    (lower, upper) = bounds arr
    mid            = midpoint lower upper

    left           = ixmap (lower, mid) id arr
    right          = ixmap (mid + 1, upper) id arr

-- yuck.