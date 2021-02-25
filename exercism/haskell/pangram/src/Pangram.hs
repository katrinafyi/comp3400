module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram text = 26 == length (nub filtered)
    where filtered = filter isAsciiLower $ toLower <$> text
