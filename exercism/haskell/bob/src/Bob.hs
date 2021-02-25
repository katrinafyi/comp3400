module Bob (responseFor) where

import Data.Char
import qualified Data.Text as T

isYelling :: String -> Bool
isYelling s = (toUpper <$> s) == s && any isUpper s

isQuestion :: String -> Bool
isQuestion s = last s == '?'
-- potentially undefined if s empty

reply :: Bool -> Bool -> Bool -> String
reply yell ask empty
    | empty         = "Fine. Be that way!"
    | yell && ask   = "Calm down, I know what I'm doing!"
    | yell          = "Whoa, chill out!"
    | ask           = "Sure."
    | otherwise     = "Whatever."

responseFor :: String -> String
responseFor xs =
    let str      = (T.unpack . T.strip . T.pack) xs
        yelling  = isYelling str
        asking   = isQuestion str
        empty    = null str
    in reply yelling asking empty

