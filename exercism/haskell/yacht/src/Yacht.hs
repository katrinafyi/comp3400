module Yacht
  ( yacht
  , Category(..)
  ) where

import           Data.List

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

data Die = D1 | D2 | D3 | D4 | D5 | D6
    deriving (Eq, Show, Ord, Enum, Bounded)

toDie :: Int -> Maybe Die
toDie 1 = Just D1
toDie 2 = Just D2
toDie 3 = Just D3
toDie 4 = Just D4
toDie 5 = Just D5
toDie 6 = Just D6
toDie _ = Nothing

toVal :: Die -> Int
toVal D1 = 1
toVal D2 = 2
toVal D3 = 3
toVal D4 = 4
toVal D5 = 5
toVal D6 = 6

filterDice :: Category -> [Die] -> [Die]
filterDice cat dice =
  let filterWith :: (Die -> Bool) -> [Die]
      filterWith f = filter f dice

      freq :: Die -> Int
      freq n = length $ filterWith (== n)

      freqs = sort $ filter (> 0) $ freq <$> [minBound .. maxBound]
  in  case cat of
        Ones        -> filterWith (== D1)
        Twos        -> filterWith (== D2)
        Threes      -> filterWith (== D3)
        Fours       -> filterWith (== D4)
        Fives       -> filterWith (== D5)
        Sixes       -> filterWith (== D6)
        FullHouse   -> if freqs == [2, 3] then dice else []
        FourOfAKind -> take 4 $ filterWith ((>= 4) . freq)
        LittleStraight ->
          if sort dice == [D1, D2, D3, D4, D5] then dice else []
        BigStraight -> if sort dice == [D2, D3, D4, D5, D6] then dice else []
        Choice      -> dice
        Yacht       -> filterWith ((== 5) . freq)

scoreDice :: Category -> [Die] -> Int
scoreDice cat dice =
  let filtered = filterDice cat dice
      ifValid :: Int -> Int
      ifValid n = if null filtered then 0 else n
  in  case cat of
        LittleStraight -> ifValid 30
        BigStraight    -> ifValid 30
        Yacht          -> ifValid 50
        _              -> sum $ toVal <$> filtered

yacht :: Category -> [Int] -> Int
yacht c ns = case traverse toDie ns of
  Just dice -> scoreDice c dice
  Nothing   -> 0
