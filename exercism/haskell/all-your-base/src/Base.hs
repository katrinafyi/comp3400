module Base (Error(..), rebase) where

import Data.Functor.Identity
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (fromMaybe)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

newtype Base a = Base a deriving (Show, Eq)

-- coefficients in MOST significant first, e.g. 42 in base 10 is Based 10 [4,2].
data Based a = Based { base :: Base a, coeffs :: [a] } deriving (Show, Eq)

liftBased :: ([a] -> [a]) -> Based a -> Based a
liftBased f (Based b ns) = Based b (f ns)

type BasedEither a b = Either (Error a) b

toBase :: Integral a => a -> Maybe (Base a)
toBase b
    | b <= 0 = Nothing
    | otherwise = Just (Base b)

digitToBased :: Integral a => Base a -> a -> BasedEither a a
digitToBased (Base b) n
    | 0 <= n && n < b = Right n
    | otherwise = Left (InvalidDigit n)

listToBased :: Integral a => Base a -> [a] -> BasedEither a (Based a)
listToBased b = fmap (Based b) . traverse (digitToBased b)

basedToNum :: Integral a => Based a -> a
basedToNum = foldl' <$> go . base <*> const 0 <*> coeffs
    where go :: Integral a => Base a -> a -> a -> a
          go (Base b) rest n = b * rest + n

numToBased :: Integral a => Base a -> a -> Based a
numToBased (Base b) n
    | n == 0 = Based (Base b) []
    | otherwise = liftBased (r:) (numToBased (Base b) q)
    where (q,r) = n `quotRem` b

rebase :: Integral a => a -> a -> [a] -> BasedEither a [a]
rebase inputBase outputBase inputDigits = do
    i <- maybe (Left InvalidInputBase) Right $ toBase inputBase
    o <- maybe (Left InvalidOutputBase) Right $ toBase outputBase
    based <- listToBased i inputDigits
    pure $ coeffs . numToBased o . basedToNum $ based
