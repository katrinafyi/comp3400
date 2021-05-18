module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import           Data.Char (ord, chr)
import           System.Random (Random(randomR), RandomGen, newStdGen)
import           Control.Monad (replicateM)
import           Control.Monad.Trans.State.Strict (evalState, state, State)

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

randomLowercase :: RandomGen g => State g Char
randomLowercase = state $ randomR ('a', 'z')

randomKey :: RandomGen g => State g [Char]
randomKey = do
  n <- state $ randomR (100, 1000)
  replicateM n randomLowercase

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- evalState randomKey <$> newStdGen
  pure (key, caesarEncode key text)
