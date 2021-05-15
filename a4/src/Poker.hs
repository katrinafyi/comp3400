module Poker where

--- do not change anything above this line ---

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

Suppose the following datatype for representing a standard deck of cards and
    data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord)
    data Rank = Numeric Int | Jack | Queens | King | Ace deriving (Eq, Ord)
    data Card = NormalCard Rank Suit | Joker deriving Eq

Your task is to determine the HAND RANKING of hand :: List[Card].  The HAND
RANKINGS in DESCENDING ORDER is given by:
    FiOAK -- five of a kind
    StFl  -- straight flush
    FoOAK -- four of a kind
    FuHo  -- full house
    Fl    -- flush
    St    -- straight
    TrOAK -- three of a kind
    TwPr  -- two pair
    OnPr  -- one pair
    HiCa  -- high card
where the definitions of the above are here: en.wikipedia.org/wiki/List_of_poker_hands
(Ignore the (**) note that says "Category does not exist under ace-to-five low rules")

Supposing
    data HandRanking = FiOAK | StFl | FoOAK | FuHo | Fl | St | TrOAK | TwPr | OnPr | HiCa deriving (Show, Eq, Ord)
write a function
    ranking :: (Card, Card, Card, Card, Card) -> HandRanking
that returns the GREATEST ranking among a hand of cards.


NOTES:

1/  Do *not* assume hands are drawn from a standard deck.  That is, presume any
    card can appear in duplicate.  In particular, assume any hand can have an
    arbitrary numbers of jokers in it.

2/  An Ace can be considered to have numeric rank 1 for the purposes of
    forming a straight or straight flush.

EXAMPLE
> ranking (Joker, Joker, Joker, Joker, Joker)
FiOAK

> ranking ((NormalCard Ace Hearts),
    (NormalCard (Numeric 2) Hearts),
    (NormalCard (Numeric 3) Hearts),
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))
StFl

> ranking (Joker,
    (NormalCard (Numeric 2) Hearts),
    (NormalCard (Numeric 2) Spades),
    (NormalCard (Numeric 3) Hearts),
    Joker)
FoOAK
-- NOT FuHo because Full House has lower rank.
--}

import           Data.List (sort, group)
import           Data.List.NonEmpty (NonEmpty, (<|))
import           Data.Maybe (mapMaybe)


data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq, Ord)
data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Eq, Ord)
data Card = NormalCard Rank Suit | Joker deriving Eq

data HandRanking = FiOAK | StFl | FoOAK | FuHo | Fl | St | TrOAK | TwPr | OnPr | HiCa deriving (Show, Eq, Ord)


data Value = AL | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | AH
  deriving (Show, Eq, Ord, Enum, Bounded)

data Hand = Hand Int [Value] [Suit]
  deriving (Show)

rankToValue :: Rank -> NonEmpty Value
rankToValue (Numeric 2)  = pure C2
rankToValue (Numeric 3)  = pure C3
rankToValue (Numeric 4)  = pure C4
rankToValue (Numeric 5)  = pure C5
rankToValue (Numeric 6)  = pure C6
rankToValue (Numeric 7)  = pure C7
rankToValue (Numeric 8)  = pure C8
rankToValue (Numeric 9)  = pure C9
rankToValue (Numeric 10) = pure C10
rankToValue Jack         = pure J
rankToValue Queen        = pure Q
rankToValue King         = pure K
rankToValue Ace          = AL <| pure AH
rankToValue (Numeric x)  = error $ "unknown numeric card rank: " ++ show x

toValueAndSuit :: Card -> Maybe (NonEmpty Value, NonEmpty Suit)
toValueAndSuit (NormalCard r s) = Just (rankToValue r, pure s)
toValueAndSuit Joker = Nothing

toHands :: [Card] -> NonEmpty Hand
toHands cs = do
  let cards = mapMaybe toValueAndSuit cs
  let jokers = length cs - length cards
  vs <- traverse fst cards
  ss <- traverse snd cards
  pure $ Hand jokers (sort vs) (sort ss)

range :: (Enum a, Ord a) => [a] -> Int
range [] = 0
range xs = fromEnum high - fromEnum low
  where
    low = minimum xs
    high = maximum xs

frequencies :: Eq a => [a] -> [Int]
frequencies = sort . fmap length . group

rankHand :: Hand -> HandRanking
rankHand (Hand jokers ranks suits)
  | maxCount == 5                  = FiOAK
  | isConsecutive && sameSuit      = StFl
  | maxCount == 4                  = FoOAK
  | maxCount == 3 && numRanks <= 2 = FuHo  -- [2, 3]
  | sameSuit                       = Fl
  | isConsecutive                  = St
  | maxCount == 3                  = TrOAK -- [1, 1, 3]
  | maxCount == 2 && numRanks <= 3 = TwPr  -- [1, 2, 2]
  | maxCount == 2                  = OnPr  -- [1, 1, 1, 2]
  | otherwise                      = HiCa
  where
    rankCounts = frequencies ranks
    suitCounts = frequencies suits

    numCards = length ranks
    maxCount = jokers + maximum (0:rankCounts)

    numRanks = length rankCounts
    sameSuit = length suitCounts <= 1

    isDistinct = numRanks == numCards
    isConsecutive = isDistinct && range ranks + 1 - numRanks <= jokers


bestRank :: [Card] -> HandRanking
bestRank = minimum . fmap rankHand . toHands


cardList :: (Card, Card, Card, Card, Card) -> [Card]
cardList (c1, c2, c3, c4, c5) = [c1, c2, c3, c4, c5]

ranking :: (Card, Card, Card, Card, Card) -> HandRanking
ranking = bestRank . cardList


-- normal straight flush
h1 :: (Card, Card, Card, Card, Card)
h1 = ((NormalCard Ace Hearts),
    (NormalCard (Numeric 2) Hearts),
    (NormalCard (Numeric 3) Hearts),
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))

-- straight flush with joker in middle
h2 :: (Card, Card, Card, Card, Card)
h2 = ((NormalCard Ace Hearts),
    (NormalCard (Numeric 2) Hearts),
    Joker,
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))

-- flush with joker
h3 :: (Card, Card, Card, Card, Card)
h3 = ((NormalCard Ace Hearts),
    (NormalCard Ace Hearts),
    Joker,
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))

-- all jokers
j :: (Card, Card, Card, Card, Card)
j = (Joker, Joker, Joker, Joker, Joker)