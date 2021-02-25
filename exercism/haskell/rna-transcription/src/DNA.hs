module DNA (toRNA) where

complement :: Char -> Either Char Char
complement c = case c of
    'G' -> Right 'C'
    'C' -> Right 'G'
    'T' -> Right 'A'
    'A' -> Right 'U'
    _   -> Left c

toRNA :: String -> Either Char String
toRNA = traverse complement
