module Dominoes (chain) where

import           Data.List (find, delete)
import           Data.Tuple (swap)

newtype Domino = Domino { getDomino :: (Int, Int) }
  deriving Show

instance Eq Domino where
  Domino d1 == Domino d2 = d1 == d2 || d1 == swap d2

-- | Returns true if the given dominos could form an adjacent pair.
isPair :: Domino -> Domino -> Bool
isPair (Domino d1) (Domino d2) = snd d1 == fst d2

-- | Returns true if the given list of dominos forms a full circular chain.
isChain :: [Domino] -> Bool
isChain ds = all (uncurry isPair) $ zip ds ds'
  where
    ds' = drop 1 $ cycle ds

-- | Returns true if the number appears on the given domino.
hasNumber :: Int -> Domino -> Bool
hasNumber n (Domino (x, y)) = n == x || n == y

-- | If necessary, flips the domino so n appears in the left position.
flipDomino :: Int -> Domino -> Domino
flipDomino n (Domino d)
  | fst d == n = Domino d
  | otherwise = Domino (swap d)

-- | Returns all partial chains which could appear following the given first domino.
-- The first domino is not contained in any of the returned chains.
-- A partial chain is a chain without the requirement that the last domino
-- "wraps around" to the first.
chainFrom :: Domino -> [Domino] -> [[Domino]]
chainFrom (Domino (_, n)) ds = do
  first <- filter (hasNumber n) ds
  let first' = flipDomino n first
  let ds' = delete first ds  -- deletes using Domino's Eq isntance.
  chainFromFirst (first':ds')

-- | Returns all partial chains which could be constructed from the given dominos.
-- The first domino in the list is taken as the first domino of the chain (arbitrarily).
chainFromFirst :: [Domino] -> [[Domino]]
chainFromFirst (d1:d2:ds) = (d1:) <$> chainFrom d1 (d2:ds)
chainFromFirst ds = pure ds
-- Ensure that chainFrom is called with a non-empty list, otherwise it will
-- always return an empty list.

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain = fmap (fmap getDomino) . find isChain . chainFromFirst . fmap Domino
