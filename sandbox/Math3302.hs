module Math3302 where

import Data.Maybe
import Data.List.NonEmpty(NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Char (ord, chr)
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

infixl 7 %
(%) :: Integral a => a -> a -> a
(%) = mod

modInvs :: Integral a => a -> a -> [a]
n `modInvs` d = filter (\x -> x*n `mod` d == 1) [0..d]

getSingleton :: [a] -> Maybe a
getSingleton [x] = Just x
getSingleton _ = Nothing

modInv :: (Show a, Integral a) => a -> a -> a
n `modInv` d =
  fromMaybe (error $ show n ++ " has multiple inverses modulo " ++ show d)
  $ getSingleton
  $ N.toList
  $ fromMaybe (error $ show n ++ " has no inverses modulo " ++ show d)
  $ nonEmpty
  $ modInvs n d

infixl 8 %-
(%-) :: (Show a, Integral a) => a -> a -> a
(%-) = modInv

toZ26 :: String -> [Int]
toZ26 = fmap (subtract (ord 'A') . ord)

fromZ26 :: [Int] -> String
fromZ26 = fmap (chr . (+ ord 'a') . (`mod` 26))