module Snake where

import           Data.List (permutations)
import           Data.Ord (comparing)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Foldable (maximumBy)
import           Data.Maybe (mapMaybe)

data SnakeWord = SnakeWord { start :: Char, end :: Char, word :: String }
  deriving (Eq, Show)

newtype Snake = Snake { unSnake :: [SnakeWord] }
  deriving (Eq, Show)

snakeLength :: Snake -> Int
snakeLength = length . unSnake

snakeStrings :: Snake -> [String]
snakeStrings = fmap word . unSnake

toSnakeWord :: String -> Maybe SnakeWord
toSnakeWord w@(x:_) = Just $ SnakeWord x (last w) w
toSnakeWord [] = Nothing

isSnakePair :: SnakeWord -> SnakeWord -> Bool
isSnakePair a b = end a == start b

prefixSnake :: [SnakeWord] -> Snake
prefixSnake [] = Snake []
prefixSnake ws'@(w:ws) = Snake (w:rest)
  where
    pairs = zip ws' ws
    rest = snd <$> takeWhile (uncurry isSnakePair) pairs

longestSnake :: [SnakeWord] -> Snake
longestSnake =
  maximumBy (comparing snakeLength) . fmap prefixSnake . permutations

snake :: [String] -> [String]
snake = snakeStrings . longestSnake . mapMaybe toSnakeWord
