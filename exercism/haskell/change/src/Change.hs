module Change (findFewestCoins) where

import Data.List (delete, minimumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f = (>>= (return . f))

instance Applicative (State s) where
  pure = return

  sf <*> sa = do
    f <- sf
    f <$> sa

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
findCoins' k@(target, coins) = do
    x <- sequence $ do
        c <- coins
        val <- takeWhile (<= target) [c, c+c..]
        let rest = findCoins' (target - val, delete c coins)
        -- fmap (replicate (fromInteger $ val `div` c) c ++)
        --fmap (replicate (fromInteger $ val `div` c) c ++) <$> x
        pure $ fmap (replicate (fromInteger $ val `div` c) c ++) <$> rest
    m <- getState
    let cached = M.lookup k m
    let computed = shortest $ catMaybes x
    let result = fromMaybe computed cached
    putState $ M.insert k result m
    pure result

-- findCoins :: Integer -> [Integer] -> [[Integer]]
-- findCoins 0 _ = [[]]
-- findCoins _ [] = []
-- findCoins target coins = do
--     c <- coins
--     val <- takeWhile (<= target) [c, c+c..]
--     (replicate (fromInteger $ val `div` c) c ++) <$> findCoins (target - val) (delete c coins)

-- memoise :: Ord a => (a -> b) -> a -> M.Map a b -> (b, M.Map a b)
-- memoise f k m =

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target = undefined --shortest . findCoins target

shortest :: [[a]] -> Maybe [a]
shortest [] = Nothing
shortest xs = Just $ minimumBy (comparing length) xs