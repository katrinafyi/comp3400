module Brackets (arePaired, matchPairs) where

import           Data.Maybe

data BracketType = Round
                 | Square
                 | Curly
  deriving (Eq, Show)

data Bracket = L BracketType
             | R BracketType
  deriving (Eq, Show)

toBracket :: Char -> Maybe Bracket
toBracket '(' = Just $ L Round
toBracket '{' = Just $ L Curly
toBracket '[' = Just $ L Square
toBracket ')' = Just $ R Round
toBracket '}' = Just $ R Curly
toBracket ']' = Just $ R Square
toBracket _ = Nothing

other :: Bracket -> Bracket
other (L x) = R x
other (R x) = L x

-- | Checks whether the given brackets form a left-right pair.
isPair :: Bracket -> Bracket -> Bool
isPair (L x) (R y) = x == y
isPair _ _ = False

-- | Given a list of brackets, matches bracket pairs and returns the unmatched brackets.
matchPairs :: [Bracket] -> [Bracket]
matchPairs = go []
  where
    -- | Finds matches by keeping track of opened brackets.
    -- First argument is stack of unmatched tokens, most recent first.
    -- Second argument is string to process.
    go :: [Bracket] -> [Bracket] -> [Bracket]
    go (l:ls) (r:rs)
      | isPair l r = go ls rs
      | otherwise = go (r:l:ls) rs
    go [] (r:rs) = go [r] rs
    go ls [] = reverse ls

-- | Fold through a list of tokens matching and removing pairs as they appear.
-- Result is list of unmatched tokens.
-- We can do this because matchPairs above considers one element "r" at a time
-- while accumulating something in ls. This is exactly the pattern of a fold.
matchPairs' :: Bracket -> [Bracket] -> [Bracket]
matchPairs' x (y:ys)
  | isPair x y = ys
matchPairs' x ys = x:ys

arePaired :: String -> Bool
arePaired = null . foldr matchPairs' [] . mapMaybe toBracket
