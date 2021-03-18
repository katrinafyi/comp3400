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
  let diceWhere :: (Die -> Bool) -> [Die]
      diceWhere = flip filter dice

      diceIf :: Bool -> [Die]
      diceIf = diceWhere . const

      freq :: Die -> Int
      freq n = length $ diceWhere (== n)

      freqs = sort $ freq <$> nub dice
  in  case cat of
        Ones           -> diceWhere (== D1)
        Twos           -> diceWhere (== D2)
        Threes         -> diceWhere (== D3)
        Fours          -> diceWhere (== D4)
        Fives          -> diceWhere (== D5)
        Sixes          -> diceWhere (== D6)
        FullHouse      -> diceIf $ freqs == [2, 3]
        FourOfAKind    -> take 4 $ diceWhere ((>= 4) . freq)
        LittleStraight -> diceIf $ sort dice == [D1 .. D5]
        BigStraight    -> diceIf $ sort dice == [D2 .. D6]
        Choice         -> dice
        Yacht          -> diceIf $ freqs == [5]

scoreDice :: Category -> [Die] -> Int
scoreDice cat dice =
  let filtered = filterDice cat dice
  in  if null filtered
        then 0
        else case cat of
          LittleStraight -> 30
          BigStraight    -> 30
          Yacht          -> 50
          _              -> sum $ toVal <$> filtered

yacht :: Category -> [Int] -> Int
yacht c ns = case traverse toDie ns of
  Just dice -> scoreDice c dice
  Nothing   -> 0
