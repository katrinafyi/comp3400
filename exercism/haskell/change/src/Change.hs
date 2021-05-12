module Change (findFewestCoins) where

import Control.Monad (guard)
import Data.Maybe (listToMaybe, maybeToList)
import Data.List (delete, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)

findCoins :: Integer -> [Integer] -> [[Integer]]
findCoins 0 _ = [[]]
findCoins _ [] = []
findCoins target coins = do
    c <- coins
    val <- takeWhile (<= target) [c, c+c..]
    rest <- findCoins (target - val) (delete c coins)
    pure $ replicate (fromInteger $ val `div` c) c ++ rest

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = shortest $ findCoins target coins

shortest :: [[a]] -> Maybe [a]
shortest [] = Nothing
shortest xs = Just $ minimumBy (comparing length) xs