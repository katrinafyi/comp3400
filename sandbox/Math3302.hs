module Math3302 where

n :: Int
n = 8

a = 2

alphabet :: [Char]
alphabet = take a $ ['a'..]

words' :: [[Char]]
words' = sequence $ replicate n alphabet

w :: [Char]
w = replicate n 'a'

distance :: Eq a => [a] -> [a] -> Int
distance a b = length $ filter id $ zipWith (/=) a b

binom :: Int -> Int -> Int
binom = loop 1 1
  where
    loop rn rd _ 0 = rn `div` rd
    loop _  _  0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1)


s' d = filter ((<= d) . distance w) words'

sum' :: Int -> Int
sum' d = sum $ (\k -> (a-1)^k * binom n k) <$> [0..d]