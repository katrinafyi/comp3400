module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

divides :: Int -> Int -> Bool
d `divides` n = n `mod` d == 0

aliquotSum :: Int -> Int
aliquotSum n = sum $ filter (`divides` n) [1..n-1]

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | otherwise = Just $ case compare (aliquotSum n) n of
        EQ -> Perfect
        GT -> Abundant
        LT -> Deficient
