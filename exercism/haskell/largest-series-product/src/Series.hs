module Series (Error(..), largestProduct) where

import           Data.Foldable (foldl', Foldable(foldMap'))
import           Data.Semigroup (Max(Max))

data Error = InvalidSpan
           | InvalidDigit Char
  deriving (Show, Eq)

toDigit :: Char -> Either Error Integer
toDigit '1' = Right 1
toDigit '2' = Right 2
toDigit '3' = Right 3
toDigit '4' = Right 4
toDigit '5' = Right 5
toDigit '6' = Right 6
toDigit '7' = Right 7
toDigit '8' = Right 8
toDigit '9' = Right 9
toDigit '0' = Right 0
toDigit x = Left $ InvalidDigit x

-- | Returns Just the first n elements of the list or Nothing otherwise.
-- Returns takeExact n also returns Nothing for negative n.
takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _ [] = Nothing
takeExact n (x:xs)
  | n < 0 = Nothing
  | otherwise = (x:) <$> takeExact (n - 1) xs

-- | Returns a sliding window of size n across the list.
-- No sublists of length != n are returned; if the list contains less than n
-- elements, the returned list will be empty. If n is 0, the return value
-- contains a single empty list. If n is negative, an empty list is returned.
window :: Int -> [a] -> [[a]]
window 0 _ = [[]]
window _ [] = []
window n (x:xs) = case takeExact n (x:xs) of
  Just chunk -> chunk : window n xs
  Nothing    -> []

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = do
  ns <- traverse toDigit digits
  case foldMap' (Just . Max) . fmap product . window size $ ns of
    Just (Max x) -> Right x
    Nothing      -> Left InvalidSpan
