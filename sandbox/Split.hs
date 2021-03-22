module Split where

split :: String -> [String]
split []         = []
split (' ' : xs) = split xs
split xs         = first : split rest where (first, rest) = span (/= ' ') xs
