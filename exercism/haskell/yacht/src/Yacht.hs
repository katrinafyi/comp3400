module Yacht (yacht, Category(..)) where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

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

-- | Dice is a list of exactly six integers in ascending order
data Dice = Dice [Int] deriving (Eq, Show)

toDice :: [Int] -> Maybe Dice
toDice ns = do
    guard $ length ns == 5
    Just $ Dice $ sort ns

-- | counts how many occurences of the given value occur in the list.
frequency :: Ord a => [a] -> Map.Map a Int
frequency a = foldr (\k -> Map.insertWith (+) k 1) Map.empty a

-- | returns the counts of elements in the list, ascending.
--
-- example: counts [100, 200, 200] = [1, 2]
counts :: Ord a => [a] -> [Int]
counts = sort . Map.elems . frequency

-- | given a category, returns the
catRequires :: Category -> Dice -> Bool
catRequires c (Dice dice) =
  let
    diceCounts = counts dice
  in case c of
    FullHouse -> diceCounts == [2,3]
    FourOfAKind -> maximum diceCounts >= 4
    LittleStraight -> dice == [1, 2, 3, 4, 5]
    BigStraight -> dice == [2, 3, 4, 5, 6]
    Yacht -> diceCounts == [5]
    _ -> True


catScore :: Category -> Dice -> Int
catScore c (Dice dice) =
  let
    diceFreq = frequency dice
    freq :: Int -> Int
    freq a = Map.findWithDefault 0 a diceFreq
  in case c of
    Ones -> freq 1
    Twos -> 2 * freq 2
    Threes -> 3 * freq 3
    Fours -> 4 * freq 4
    Fives -> 5 * freq 5
    Sixes -> 6 * freq 6
    FullHouse -> sum $ filter ((>= 2) . freq) dice
    FourOfAKind -> sum $ take 4 $ filter ((>= 4) . freq) dice
    LittleStraight -> 30
    BigStraight -> 30
    Choice -> sum dice
    Yacht -> 50

yacht :: Category -> [Int] -> Int
yacht c ns = fromMaybe 0 $ do
    dice <- toDice ns
    guard $ catRequires c dice
    Just $ catScore c dice
