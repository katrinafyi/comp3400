module Base (Error(..), rebase) where

import           Data.Foldable (Foldable(foldr'))
import           Data.List (unfoldr)
import           Data.Tuple (swap)
import           Control.Applicative (liftA2)
import           Data.Void (Void, absurd)


data Error a = InvalidInputBase
             | InvalidOutputBase
             | InvalidDigit a
  deriving (Show, Eq)

newtype Base a = Base a
  deriving (Show, Eq)

-- | Based.
-- coefficients in LEAST significant first, e.g. 42 in base 10 is Based 10 [2,4].
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

digitsToBased :: Integral a => Base a -> [a] -> BasedEither a (Based a)
digitsToBased b =
  fmap (Based b . reverse . dropWhile (== 0))
  . traverse (digitToBased b)

basedToDigits :: Integral a => Based a -> [a]
basedToDigits = reverse . coeffs

multAdd :: Num a => a -> a -> a -> a
multAdd b q r = q * b + r

basedToNum :: Integral a => Based a -> a
basedToNum (Based (Base b) cs) = foldr' (flip $ multAdd b) 0 cs

numToBased :: Integral a => Base a -> a -> Based a
numToBased b = Based b . unfoldr (go b)
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
  based <- digitsToBased i inputDigits
  pure $ basedToDigits . numToBased o . basedToNum $ based


foldAp :: (Foldable t, Applicative f) => (a -> b -> b) -> b -> t (f a) -> f b
foldAp f b = foldr (liftA2 f) (pure b)

foo :: Monad m => (c -> m a) -> (a -> b -> b) -> b -> [c] -> m b
foo g f b = foldAp f b . fmap g

b :: Base Integer
b = Base 10

{-
x^0 = 1
x^Void = 1
Void -> x

a -> Void

0^A = 0
-}

g :: (a, a -> Void) -> Void
g (a, fa) = fa a

h :: a -> (a -> Void) -> Void
h a fa = fa a

-- f :: Void -> a
-- f a = case a of ()

type Not a = a -> Void

-- 0 ^ (p^(0^p) + (0^p)^p)
-- t7 :: (p -> (p -> Void)) -> ((p -> Void) -> p) -> Void
t7 :: (p -> (p -> Void)) -> ((p -> Void) -> p) -> Void
t7 f g = p2v p
  where p2v = \p -> f p p
        p = g p2v