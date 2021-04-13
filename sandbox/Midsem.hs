module Midsem where

import Data.Maybe ( isJust )
import Data.Either (fromLeft)
import Data.Ord
import Control.Applicative
import Data.Monoid

-- q5

{-
> [(x,y) | x <- [1,2], y <- [3,4]]
[(1,3),(1,4),(2,3),(2,4)]
-}

a = [(x,y) | x <- [1,2], y <- [3,4]]
b = [flip (,) y | y <- [3, 4]]
c = concat [fmap ($x) b | x <- [1,2]]

-- d = [x | x <- [[1, 2], [3, 4]]]
-- e = [(x,y) | [x,y] <- d]

-- q7

data ExtOrd a = NegInf | Finite a | PosInf deriving (Eq,Ord)

ordered :: Ord a => [a] -> Bool
ordered = (/= NegInf) . foldr go PosInf
  where
    go :: Ord a => a -> ExtOrd a -> ExtOrd a
    go a1 a2 | Finite a1 < a2 = Finite a1
    go _ _ = NegInf

ordered2 :: Ord a => [a] -> Bool
ordered2 = ordered . fmap Down

-- q8

foo :: [Int] -> [Int]
foo []       = []
foo [x]      = [abs x]
foo (x:y:xs) = (abs x) : y : foo xs

bar :: [Int] -> [Int]
bar []       = []
bar [x]      = [x+1]
bar (x:y:xs) = (x+1) : y : bar xs

vrb :: (a -> a) -> [a] -> [a]
vrb _ [] = []
vrb f [x] = [f x]
vrb f (x:y:xs) = f x : y : vrb f xs

p_vrb :: [Int] -> Bool
p_vrb xs = and [ (foo xs == vrb abs xs), (bar xs == vrb (+1) xs) ]

-- q9

data Suit = Hearts | Clubs | Diamonds | Spades deriving Eq
data Rank = Numeric Int | Jack | Queens | King | Ace deriving Eq
data Card = NormalCard Rank Suit | Joker deriving Eq

isAce :: Card -> Bool
isAce (NormalCard Ace _) = True
isAce _ = False

isJoker :: Card -> Bool
isJoker Joker = True
isJoker _ = False

-- we would like a function which returns true if a card is a joker or an ace
f1, f2, f3, f4, f5, f6 :: Card -> Bool  -- (declaring functions in advance)
f1 a = isJoker a || isAce a

-- it's easy to spaceship this
f2 = (||) <$> isJoker <*> isAce

-- but this doesn't generalise well. ideally, we would like to have an or
-- operator which works on functions, and arbitrarily many of them.
or1, or2 :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or1 f g a = f a || g a
-- spaceships
or2 f g = (||) <$> f <*> g

-- but what we really want is to generalise a boolean operator like:
-- (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- ... tada!
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)
infixr 2 |||

f3 = isJoker ||| isAce
f4 = isJoker ||| isAce ||| isJoker ||| isAce -- it generalises!

-- ok cool, but what if we had a list of functions [a -> Bool]?
-- well, looking at f4, it could become a list if we replace (|||) with (:).
-- but that means we could just as easily replace (:) with (|||) with a fold.
-- we also need a base which should be the identity for (|||), which is const True.
f5 = foldr (|||) (const False) [isJoker, isAce]

-- but there's a pattern here, all we've done is lift (||) and True into
-- the applicative (->) a. in fact, you might remember:
foldAp :: (Foldable f, Applicative g) => (a -> b -> b) -> b -> f (g a) -> g b
foldAp f b = foldr (liftA2 f) (pure b)

-- how good?
f6 = foldAp (||) False [isJoker, isAce]

-- this is pretty good but we've basically reimplemented "or" / "any".
-- let's take a step back. we have a [a -> Bool] which is somewhat hard to work with.
-- particularly, the (a ->) inside the list is inconvenient.
-- what if we had a -> [Bool] instead? can we do that?

-- of course we can. in fact, there is a very familiar prelude function to do it.
g1, g2 :: Card -> [Bool]
g1 = sequenceA [isJoker, isAce]

g2 = ([isJoker, isAce] <*>) . pure

-- now, we have a list of Bool so we can just use or.
f7 :: Card -> Bool
f7 = getAny . foldMap Any . sequence [isJoker, isAce]

-- and we'll leave it at that.


-- q10
data Expr = X | Num Int | BinOp Op Expr Expr deriving (Eq, Show)
data Op = Add | Mul | Subtract deriving (Eq, Show)

-- 100 - X = 100 + (-1) * X

neg :: Expr -> Expr
neg = BinOp Mul (Num (-1))

removeSub :: Expr -> Expr
removeSub (BinOp op x y) =
  case op of
    Subtract -> BinOp Add x' (neg y')
    _        -> BinOp op x' y'
  where
    x' = removeSub x
    y' = removeSub y
removeSub x = x

-- q11

rev :: [a] -> [a]
rev = h_rev []
  where
    h_rev :: [a] -> [a] -> [a]
    h_rev ys [] = ys
    h_rev ys (x:xs) = h_rev (x:ys) xs