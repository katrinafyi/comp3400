module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n =
    let s = n * (n + 1) `div` 2
    in s * s

sumOfSquares :: Integral a => a -> a
sumOfSquares n =
    n * (n + 1) * (2*n + 1) `div` 6
