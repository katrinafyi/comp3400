module SumOfMultiples (sumOfMultiples) where

-- | returns an infinite list of positive multiples of the given number
multiples :: Integer -> [Integer]
multiples n = [n, n+n ..]

-- sumOfMultiples :: [Integer] -> Integer -> Integer
-- sumOfMultiples factors limit =
--     sum $ nub $ concat $ takeWhile (<= limit) . multiples <$> factors

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    sum $ filter isMultiple [1..limit-1]
  where
    isMultiple :: Integer -> Bool
    isMultiple n = any (\f -> n `mod` f == 0) $ nonzeroFactors
    nonzeroFactors = filter (/= 0) factors
