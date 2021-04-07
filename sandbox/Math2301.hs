module Math2301 where

import qualified Data.Map as Map
import Data.Map (Map)

newtype Cycle = Cycle { unCycle :: [Int] }
  deriving (Eq, Show)

newtype Permutation = Permutation { unPerm :: [Cycle] }
  deriving (Show)

makeCycle :: [Int] -> Cycle
makeCycle [] = Cycle []
makeCycle ns = Cycle c
  where
    c = take (length ns) $ dropWhile (/= m) $ cycle ns

    m = minimum ns

makePerm :: [Cycle] -> Permutation
makePerm = Permutation
  . fmap Cycle
  . sortOn length
  . filter ((> 1) . length)
  . fmap unCycle

getFunction :: Permutation -> (Int -> Int)
getFunction (Permutation cycles) n = fromMaybe n $ do
    cycle <- find (n `elem`) . fmap unCycle $ cycles
    let nextMaybe = listToMaybe $ dropWhile (/= n) cycle
    let headMaybe = listToMaybe cycle
    nextMaybe <|> headMaybe