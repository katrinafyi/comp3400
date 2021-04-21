module Series (slices) where

import Data.Char (digitToInt)

takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _ [] = Nothing
takeExact n (x:xs)
  | n < 0 = Nothing
  | otherwise = (x:) <$> takeExact (n - 1) xs

-- | Returns a sliding window of size n across the list.
-- No sublists of length != n are returned; if the list contains less than n
-- elements, the returned list will be empty.
-- If n is negative, an empty list is returned.
window :: Int -> [a] -> [[a]]
window 0 [] = [[]]
window _ [] = []
window n (x:xs) = case takeExact n (x:xs) of
  Just chunk -> chunk : window n xs
  Nothing    -> []

slices :: Int -> String -> [[Int]]
slices n = window n . fmap digitToInt
