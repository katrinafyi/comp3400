module Change (findFewestCoins) where

import           Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as N
import           Data.Maybe (catMaybes, fromMaybe)
import           Control.Monad (ap, liftM)
import           Data.List (minimumBy)


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))

  State sf >>= g = State
    $ \s -> let (a, s') = sf s
            in runState (g a) s'

getState :: State s s
getState = State $ \s -> (s,s)

putState :: s -> State s ()
putState s = State $ const ((), s)

type MemoisedFunction a b = a -> State (M.Map a b) b

findCoins' :: MemoisedFunction (Integer, [Integer]) (Maybe [Integer])
findCoins' (0, _) = pure $ Just []
findCoins' (_, []) = pure Nothing
findCoins' k@(target, c:cs) = do
  x <- sequence
    $ do
      val <- takeWhile (<= target) [0, c..]
      let rest = findCoins' (target - val, cs)
      -- fmap (replicate (fromInteger $ val `div` c) c ++)
      --fmap (replicate (fromInteger $ val `div` c) c ++) <$> x
      pure $ fmap (replicate (fromInteger $ val `div` c) c ++) <$> rest
  m <- getState
  let cached = M.lookup k m
  let computed = shortest $ catMaybes x
  let result = fromMaybe computed cached
  putState $ M.insert k result m
  pure result

findCoins :: Integer -> [Integer] -> [[Integer]]
findCoins 0 _ = [[]]
findCoins _ [] = []
findCoins target (c:cs) = do
  val <- takeWhile (<= target) [0, c..]
  rest <- findCoins (target - val) cs
  let n = fromInteger $ val `div` c
  pure $ replicate n c ++ rest

headMaybe :: [a] -> Maybe a
headMaybe = fmap N.head . N.nonEmpty

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = headMaybe $ findCoins target coins
-- findFewestCoins target coins = fst $ runState (findCoins' (target, coins)) M.empty

shortest :: [[a]] -> Maybe [a]
shortest [] = Nothing
shortest xs = Just $ minimumBy (comparing length) xs