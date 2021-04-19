module Base (Error(..), rebase) where

import           Data.Foldable (Foldable(foldl'))
import           Data.List (unfoldr)
import           Data.Tuple (swap)

data Error a = InvalidInputBase
             | InvalidOutputBase
             | InvalidDigit a
  deriving (Show, Eq)

newtype Base a = Base a
  deriving (Show, Eq)

-- | Based.
-- coefficients in MOST significant first, e.g. 42 in base 10 is Based 10 [4,2].
data Based a = Based { base :: Base a, coeffs :: [a] }
  deriving (Show, Eq)

type BasedEither a b = Either (Error a) b

toBase :: Integral a => a -> Maybe (Base a)
toBase b
  | b > 1 = Just (Base b)
  | otherwise = Nothing

digitToBased :: Integral a => Base a -> a -> BasedEither a a
digitToBased (Base b) n
  | 0 <= n && n < b = Right n
  | otherwise = Left (InvalidDigit n)

listToBased :: Integral a => Base a -> [a] -> BasedEither a (Based a)
listToBased b = fmap (Based b) . traverse (digitToBased b)

multAdd :: Num a => a -> a -> a -> a
multAdd b q r = q * b + r

basedToNum :: Integral a => Based a -> a
basedToNum (Based (Base b) cs) = foldl' (multAdd b) 0 cs

numToBased :: Integral a => Base a -> a -> Based a
numToBased b = Based b . reverse .  unfoldr (go b)
  where
    go :: Integral a => Base a -> a -> Maybe (a, a)
    go _ 0 = Nothing
    go (Base b') n = Just $ swap (n `quotRem` b')

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right

rebase :: Integral a => a -> a -> [a] -> BasedEither a [a]
rebase inputBase outputBase inputDigits = do
  i <- maybeToEither InvalidInputBase $ toBase inputBase
  o <- maybeToEither InvalidOutputBase $ toBase outputBase
  based <- listToBased i inputDigits
  pure $ coeffs . numToBased o . basedToNum $ based
