module Hangman where

import           Data.Bool (bool)
import           Control.Monad (unless)

zipWithDefault :: (Either a b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault _ _ [] [] = []
zipWithDefault g f (a:as) (b:bs) = f a b : zipWithDefault g f as bs
zipWithDefault g _ as bs = fmap g $ fmap Left as ++ fmap Right bs

checkGuess :: String -> String -> [Bool]
checkGuess secret guess = fmap (`elem` guess) secret

maskSecret :: String -> [Bool] -> String
maskSecret = zipWith (bool '*')

guessLoop :: String -> String -> IO ()
guessLoop secret guess = unless (guess == secret)
  $ do
    putStrLn masked
    putStr "Guess word: "
    getLine >>= guessLoop secret
  where
    mask = checkGuess secret guess
    masked = maskSecret secret mask

main :: IO ()
main = do
  putStr "Enter secret word: "
  secret <- getLine
  guessLoop secret ""
  putStrLn "Correct!"

