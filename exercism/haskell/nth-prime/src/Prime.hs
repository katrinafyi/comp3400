module Prime (nth) where

import Data.Maybe

divides :: Integral a => a -> a -> Bool
d `divides` n = n `mod` d == 0

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = [1] == filter (`divides` n) [1..bound]
    where bound = floor $ sqrt $ fromIntegral n

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing
    | otherwise = listToMaybe $ drop (n-1) $ filter isPrime [1..]
