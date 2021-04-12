module Midsem where

import Data.Maybe ( isJust )
import Data.Either (fromLeft)
import Data.Ord

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

countAces :: [Card] -> Int
countAces = length . filter ((||) <$> isAce <*> isJoker)


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