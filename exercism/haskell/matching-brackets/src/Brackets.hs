module Brackets (arePaired, matchPairs) where

import Control.Monad
import Data.Maybe

otherPair :: Char -> Char
otherPair c = case c of
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    ')' -> '('
    ']' -> '['
    '}' -> '{'
    _ -> error "invalid bracket character"

openChars :: String
openChars = "([{"

closeChars :: String
closeChars = otherPair <$> openChars

isOpen :: Char -> Bool
isOpen = (`elem` openChars)

isClose :: Char -> Bool
isClose = (`elem` closeChars)

-- | attempts to match bracket sequences within the string.
--
-- generally, matchPairs returns Nothing if the given string cannot be a suffix
-- of any string with matched brackets (called "valid" strings), e.g. "[}".
-- otherwise, it returns Just rest where rest is string with as many matching
-- brackets as possible matched and removed. rest only contains closing brackets
-- indicating this could be a suffix of some string which contained opening brackets.
--
-- examples:
-- matchPairs "[}" = Nothing -- incorrectly matched brackets are always invalid.
-- matchPairs "}{" = Nothing -- unmatched open bracket cannot be suffix.
-- matchPairs "{}[]" = Just "" -- valid suffix and pairs {}[] are removed.
-- matchPairs "}[][[]]}" = Just "}}" -- matching brackets removed leaving "}}".
matchPairs :: String -> Maybe String
matchPairs [] = Just []
matchPairs (x:xs)
    | isClose x = (x:) <$> matchPairs xs
    | isOpen x = do
        rest <- matchPairs xs
        guard $ [otherPair x] == take 1 rest
        Just $ drop 1 rest
    | otherwise = matchPairs xs

arePaired :: String -> Bool
arePaired xs = fromMaybe False $ null <$> matchPairs xs
