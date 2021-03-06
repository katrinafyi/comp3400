module CollatzConjecture (collatz, collatzMaybe2) where

import Data.List

-- UNUSED
collatzStep :: Integer -> Integer
collatzStep n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

collatzMaybe :: Integer -> Maybe Integer
collatzMaybe n =
    case n of
        1 -> Nothing
        _ -> Just $ collatzStep n

countSteps :: (a -> Maybe a) -> a -> Integer -> Integer
countSteps f x0 c =
    let x = f x0
    in case x of
        Just y  -> countSteps f y (c + 1)
        Nothing -> c

-- USED
dupe :: a -> (a, a)
dupe x = (x, x)

collatzMaybe2 :: Integer -> Maybe (Integer, Integer)
collatzMaybe2 n = (\x -> (n, x)) <$> collatzMaybe n


collatz :: Integer -> Maybe Integer
collatz n = Just $ genericLength $ unfoldr collatzMaybe2 n

