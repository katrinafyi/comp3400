module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr)
import System.Random (randomRIO)
import Control.Monad (replicateM)

a :: Int
a = ord 'a'

charToInt :: Char -> Int
charToInt c = (ord c - a) `mod` 26

intToChar :: Int -> Char
intToChar n = chr $ a + (n `mod` 26)

encodeChar :: Int -> Char -> Char
encodeChar k c = intToChar $ charToInt c + k

decodeChar :: Int -> Char -> Char
decodeChar k c = intToChar $ charToInt c - k

caesarDecode :: String -> String -> String
caesarDecode = zipWith decodeChar . cycle . fmap charToInt

caesarEncode :: String -> String -> String
caesarEncode = zipWith encodeChar . cycle . fmap charToInt

randomLowercase :: IO Char
randomLowercase = randomRIO ('a', 'z')

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    len <- randomRIO (100, 1000) :: IO Int
    key <- replicateM len randomLowercase
    pure (key, caesarEncode key text)
