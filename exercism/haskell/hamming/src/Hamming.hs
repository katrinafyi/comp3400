module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs == length ys =
        Just $ length $ filter (uncurry (/=)) $ zip xs ys
    | otherwise = Nothing
