module Brackets
    ( arePaired
    , matchPairs
    ) where

import           Data.Maybe

other :: Char -> Maybe Char
other c = case c of
    '(' -> Just ')'
    '[' -> Just ']'
    '{' -> Just '}'
    ')' -> Just '('
    ']' -> Just '['
    '}' -> Just '{'
    _   -> Nothing

openChars :: String
openChars = "([{"

closeChars :: String
closeChars = catMaybes $ other <$> openChars

isOpen :: Char -> Bool
isOpen = (`elem` openChars)

isClose :: Char -> Bool
isClose = (`elem` closeChars)

isBracket :: Char -> Bool
isBracket c = isOpen c || isClose c

-- | Given a string, returns the mismatched brackets in that string.
matchPairs :: [Char] -> String
matchPairs = go []
  where
    -- | Finds matches by keeping track of opened brackets.
    -- First argument is stack of unmatched tokens, most recent first.
    -- Second argument is string to process.
    go :: [Char] -> [Char] -> String
    go ls' (r : rs) | isClose r && isPair = go ls rs
                    | isBracket r         = go (r : ls') rs
                    | otherwise           = go ls' rs
      where
        (l', ls) = splitAt 1 ls'
        isPair   = l' == maybeToList (other r)
    go ls' [] = reverse ls'

arePaired :: String -> Bool
arePaired = null . matchPairs
