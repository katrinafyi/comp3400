module Boolean (true, false, and, or, not) where

import Prelude((==), String, Int)

type Bool a = a -> a -> a
type BinaryOp a = Bool a -> Bool a -> Bool a

true :: Bool a
true x y = x

false :: Bool a
false x y = y

and :: Bool a -> Bool a -> Bool a
and p q = p q p

or :: Bool a -> Bool a -> Bool a
or p q = p p q

string :: Bool Int -> String
string x = if x 1 0 == 1 then "true" else "false"
