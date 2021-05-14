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

import           Data.List (sort, group, nub)
import           Data.List.NonEmpty (NonEmpty, (<|))
import           Data.Maybe (mapMaybe)
import           Data.Semigroup (Arg(Arg))


data Suit = Hearts | Clubs | Diamonds | Spades
  deriving (Show, Eq, Ord, Enum, Bounded)
data Rank = Numeric Int | Jack | Queens | King | Ace
  deriving (Eq, Ord)
data Card = NormalCard Rank Suit | Joker
  deriving (Eq)

data HandRanking = FiOAK | StFl | FoOAK | FuHo | Fl | St | TrOAK | TwPr | OnPr | HiCa
  deriving (Show, Eq, Ord)


data Value = AL | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | AH
  deriving (Show, Eq, Ord, Enum, Bounded)

data Hand = Hand { handValues :: [Value], handSuits :: [Suit] }
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
rankToValue Queens       = pure Q
rankToValue King         = pure K
rankToValue Ace          = AL <| pure AH
rankToValue (Numeric x)  = error $ "unknown numeric card rank: " ++ show x

toValues :: Card -> Maybe (NonEmpty Value)
toValues (NormalCard r _) = Just $ rankToValue r
toValues Joker = Nothing

toSuit :: Card -> Maybe (NonEmpty Suit)
toSuit (NormalCard _ s) = Just $ pure s
toSuit Joker = Nothing

toHands :: [Card] -> NonEmpty Hand
toHands cs = do
  vs <- sequence $ mapMaybe toValues cs
  ss <- sequence $ mapMaybe toSuit cs
  pure $ Hand (sort vs) (sort ss)

range :: (Enum a, Ord a) => [a] -> Int
range [] = 0
range xs = fromEnum high - fromEnum low
  where
    low = minimum xs
    high = maximum xs

frequencies :: Eq a => [a] -> [Int]
frequencies = sort . fmap length . group

rankHand :: Hand -> HandRanking
rankHand (Hand values suits)
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
    valCounts = frequencies values
    suitCounts = frequencies suits

    numCards = length values
    numJokers = 5 - numCards
    maxCount = numJokers + maximum (0:valCounts)

    numRanks = length valCounts
    sameSuit = length suitCounts <= 1

    isDistinct = numRanks == numCards
    isConsecutive = isDistinct && range values - numRanks <= numJokers


arg :: (a -> b) -> a -> Arg b a
arg f x = Arg (f x) x

getArgVal :: Arg a b -> a
getArgVal (Arg x _) = x

bestHand :: [Card] -> Arg HandRanking Hand
bestHand = minimum . fmap (arg rankHand) . toHands


cardList :: (Card, Card, Card, Card, Card) -> [Card]
cardList (c1, c2, c3, c4, c5) = [c1, c2, c3, c4, c5]

ranking :: (Card, Card, Card, Card, Card) -> HandRanking
ranking = getArgVal . bestHand . cardList


h1 :: (Card, Card, Card, Card, Card)
h1 = ((NormalCard Ace Hearts),
    (NormalCard (Numeric 2) Hearts),
    (NormalCard (Numeric 3) Hearts),
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))

h2 :: (Card, Card, Card, Card, Card)
h2 = ((NormalCard Ace Hearts),
    (NormalCard (Numeric 2) Hearts),
    Joker,
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))

h3 :: (Card, Card, Card, Card, Card)
h3 = ((NormalCard Ace Hearts),
    (NormalCard Ace Hearts),
    Joker,
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))

j :: (Card, Card, Card, Card, Card)
j = (Joker, Joker, Joker, Joker, Joker)