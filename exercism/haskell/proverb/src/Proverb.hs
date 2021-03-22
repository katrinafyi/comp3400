module Proverb(recite) where

window :: [a] -> [(a,a)]
window (x:y:xs) = (x, y) : window (y:xs)
window _ = []

forWant :: String -> String -> String
forWant a b  = "For want of a " ++ a ++ " the " ++ b ++ " was lost.\n"

allForWant :: String -> String
allForWant x = "And all for the want of a " ++ x ++ "."

recite :: [String] -> String
recite xs'@(x:_) =
    concatMap (uncurry forWant) (window xs') ++ allForWant x
recite _ = ""
