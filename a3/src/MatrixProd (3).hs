module MatrixProd where

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

Given two Matrices AA and BB their MATRIX PRODUCT AA x BB is defined by
    (AA x BB)[i,j] == row(i, AA) DOT col(j, BB)
where DOT is the "dot product" or "scalar product" defined by
    [x1, x2, ... xk] (DOT) [y1, y2, ... yk] = x1*y1 + x2*y2 + ... + xk*yk

=======
EXAMPLE
=======

[[1,2],[3,4],[5,6]] x [[7,8,9],[10,11,12]]
[[27,30,33],[61,68,75],[95,106,117]]

Implement matrix multiplication on the Matix type, which is just a type
synononym for [[Int]].  Your matrixMult **must be total** by utilizing the
provided Error datattype:

data Error = DimMismatch (Either Error Matrix) (Either Error Matrix) | NotAMatrix Matrix deriving Show

==============
Complication 1
==============
A matrix is a list of rows of EQUAL length.  The type [[Int]] cannot distinguish
 between a well-formed Matrix like:
    > aa = [ [1,2], [3,4], [5,6] ]
and an ill-formed one like:
    > bb = [ [1,2], [3,4,5], [6,7,8,9] ]

When encountering ill-formed matrices return the "NotAMatrix" error.

NOTE:  [[]] is NOT a matrix.  The smallest matrix is [[x]].

=======
EXAMPLE
=======

> matProd [[1,2],[3,4],[5,6]] [[1,2],[3,4,5],[6,7,8,9]]
[NotAMatrix [[1,2],[3,4,5],[6,7,8,9]]]
Left $ DimMismatch (Right [[1,2],[3,4],[5,6]]) (Left $ NotAMatrix [[1,2],[3,4,5],[6,7,8,9]])

> matProd [[1],[2,3]] [[]]
Left $ DimMismatch (Left $ NotAMatrix [[1],[2,3]]) (Left $ NotAMatrix [[]])

==============
Complication 2
==============
Matrix multiplication only works when the matrices have compatable dimension.

That is, (AA)(BB) is a well-defined operation when
    AA has J rows and K columns
    BB has K rows and L columns
then
    (AA)(BB) has J rows and L columns.

Your matProd function should return the following when encountering mismatched
dimensions on well-formed matrices
    > matProd (Right [[1,2],[3,4],[5,6]]) (Right [[7,8],[9,10],[11,12]])
    Left $ DimMismatch (Right [[1,2],[3,4],[5,6]]) (Right [[7,8],[9,10],[11,12]])
and the following when multiplying ill-formed matrices.
    > matProd (Right [[1,2],[3,4],[5,6]]) (Right [[7,8],[9],[11,12]])
    Left $ DimMismatch (Right [[1,2],[3,4],[5,6]]) (Left $ NotAMatrix [[7,8],[9],[11,12]])

==============
COMPLICATION 3
==============
When doing a sequence of matrix multiplications, you must return a recursive
Error like the following:
    > matProd (Right [[3,4,5], [1]]) (matProd (Right [[1],[1,2]]) (Right []))
    Left $ DimMismatch (Left $ NotAMatrix [[3,4,5], [1]]) (Left $ DimMismatch (Left $ NotAMatrix [[1],[1,2]]) (Left $ NotAMatrix []))

==============
COMPLICATION 4
===============
Matrix multiplication is ASSOCIATIVE.  Thereby, it will be possible to minimize
the number of errors lifted by your code.

For example:
    > matProd (Right [[1,1],[2,2]]) (matProd (Right [[1,1,1],[2,2,2]]) (Right [[1],[1,2]]))
    Left $ DimMismatch (Right [[3,3,3],[6,6,6]]) (Left $ NotAMatrix [[1],[1,2]])
and NOT
    Left $ DimMismatch (Right [[1,1],[2,2]]) (Left $ DimMismatch (Right [[1,1,1],[2,2,2]]) (Left $ NotAMatrix [[1],[1,2]]))

--}

import           Data.Functor.Classes (Show1(liftShowsPrec), showsPrec1)
import           Data.List (transpose)


data Two a = Two a a deriving Eq
instance Show1 Two where
  liftShowsPrec showP _ p fa = showParen (p > 10)
    $ showString "Two" . foldr (\a b -> showChar ' ' . showP 11 a . b) id fa

instance (Show a) => Show (Two a) where showsPrec = showsPrec1
instance Functor Two where
  fmap f (Two a b) = Two (f a) (f b)

instance Foldable Two where
  foldr f x (Two a b) = f a $ f b x


data Free f a = Pure a | Free (f (Free f a))
instance (Show a, Show1 f) => Show (Free f a) where
  showsPrec p (Pure x) = showParen (p > 10)
    $ showString "Pure " . showsPrec 11 x
  showsPrec p (Free fx) = showParen (p > 10)
    $ showString "Free " . liftShowsPrec showsPrec showList 11 fx

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap f <$> x

mapFree :: (Functor f, Functor g) => (f (Free g a) -> g (Free g a)) -> Free f a -> Free g a
mapFree f = foldFree (Free . f) Pure

foldFree :: (Functor f) => (f b -> b) -> (a -> b) -> Free f a -> b
foldFree _ b (Pure x) = b x
foldFree f b (Free x) = f $ foldFree f b <$> x


data Error = DimMismatch (Either Error Matrix) (Either Error Matrix) | NotAMatrix Matrix deriving Show

type Matrix = [[Int]]

-- | Matrix containing lists, rows, then columns.
data Mat a = Mat [[a]] Int Int | NotMat [[a]]
  deriving (Show, Eq)


toMatrix :: [[a]] -> Mat a
toMatrix m@(r@(_:_):rs)
  | all ((== cols) . length) rs = Mat m rows cols
  where
    cols = length r
    rows = length m
toMatrix m = NotMat m

toMatrixTree :: Either Error Matrix -> Free Two (Mat Int)
toMatrixTree (Left (NotAMatrix m)) = Pure $ toMatrix m
toMatrixTree (Left (DimMismatch e1 e2)) = Free
  $ Two (toMatrixTree e1) (toMatrixTree e2)
toMatrixTree (Right m) = Pure $ toMatrix m


extractLeft :: Free Two a -> (a, Maybe (Free Two a))
extractLeft (Pure x) = (x, Nothing)
extractLeft (Free (Two l r)) = (x, Just $ fuseTree l' r Nothing)
  where
    (x, l') = extractLeft l

extractRight :: Free Two a -> (a, Maybe (Free Two a))
extractRight (Pure x) = (x, Nothing)
extractRight (Free (Two l r)) = (x, Just $ fuseTree Nothing l r')
  where
    (x, r') = extractRight r


dot :: Num a => [a] -> [a] -> a
dot x y = sum $ zipWith (*) x y

maybeMult :: Num a => Mat a -> Mat a -> Maybe (Mat a)
maybeMult (Mat m1 r1 c1) (Mat m2 r2 c2)
  | c1 == r2 = Just $ Mat m' r1 c2
  where
    columns = transpose m2
    rows = m1
    m' = (\r -> dot r <$> columns) <$> rows
maybeMult _ _ = Nothing


fuseTree :: Maybe (Free Two a) -> Free Two a -> Maybe (Free Two a) -> Free Two a
fuseTree (Just l) x (Just r) = Free $ Two l $ Free (Two x r)
fuseTree (Just l) x Nothing = Free $ Two l x
fuseTree Nothing x (Just r) = Free $ Two x r
fuseTree Nothing x Nothing = x

computeProducts :: Num a => Two (Free Two (Mat a)) -> Free Two (Mat a)
computeProducts (Two l r) = case maybeMult m1 m2 of
  Just m1m2 -> fuseTree l' (Pure m1m2) r'
  Nothing   -> Free (Two l r)
  where
    (m1, l') = extractRight l
    (m2, r') = extractLeft r

-- (^^^) :: Error -> Error -> Error
-- a ^^^ b = DimMismatch (Left a) (Left b)

-- a, b, c :: Matrix
-- a = [[1]]
-- b = [[2]]
-- c = [[3]]

-- e1 = (NotAMatrix [] ^^^ ((NotAMatrix a ^^^ NotAMatrix []) ^^^ NotAMatrix c)) ^^^ (NotAMatrix b ^^^ NotAMatrix [])

twoToError :: Two (Either Error Matrix) -> Either Error Matrix
twoToError (Two x y) = Left $ DimMismatch x y

matToError :: Mat Int -> Either Error Matrix
matToError (Mat m _ _) = Right m
matToError (NotMat m) = Left $ NotAMatrix m

toMatrixError :: Free Two (Mat Int) -> Either Error Matrix
toMatrixError = foldFree twoToError matToError

matProd' :: Num a => Free Two (Mat a) -> Free Two (Mat a)
matProd' = foldFree computeProducts Pure

matProd :: Either Error Matrix -> Either Error Matrix -> Either Error Matrix
matProd x y = toMatrixError . matProd' . toMatrixTree $ Left (DimMismatch x y)
