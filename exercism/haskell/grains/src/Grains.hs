module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n
    | 1 <= n && n <= 64 = Just (2 ^ (n - 1))
    | otherwise = Nothing

total :: Integer
total = sum $ mapMaybe square [1..64]

