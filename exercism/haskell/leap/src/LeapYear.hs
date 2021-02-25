module LeapYear (isLeapYear) where

divides :: Integer -> Integer -> Bool
divides d n = n `mod` d == 0

isLeapYear :: Integer -> Bool
isLeapYear year = if 100 `divides` year then 400 `divides` year else 4 `divides` year






