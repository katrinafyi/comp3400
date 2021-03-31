module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

instance Show Clock where
  show (Clock h m) = padTime (show h) ++ ":" ++ padTime (show m)


toClock :: Int -> Int -> Clock
toClock h m = Clock (h' `mod` 24) (m `mod` 60)
  where h' = h + m `div` 60

-- bringer of npm chaos, padLeft.
padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs') x ++ xs'
  where xs' = take n xs

padTime :: String -> String
padTime = padLeft 2 '0'


fromHourMin :: Int -> Int -> Clock
fromHourMin = toClock

toString :: Clock -> String
toString = show

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock h m) = toClock (h + dh) (m + dm)
