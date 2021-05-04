module LeapYear (isLeapYear) where

divides :: Integer -> Integer -> Bool
divides d n = n `mod` d == 0

isLeapYear :: Integer -> Bool
isLeapYear y
  | 100 `divides` y = 400 `divides` y
  | otherwise = 4 `divides` y
