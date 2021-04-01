module Prop (Prop, tautology) where

{--
*DO NOT* load any modules.

You may remove the comments if you like.

A Proposition is a expression which evalutes to a boolean.

Consider the following type for encoding a Proposition:

data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop deriving Show

The proposition "p and not q" would be encoded as
  And (Var "p") (Not $ Var "q")
and can be reduced by binding "p" and "q" to booleans
  And (Var "p") (Not $ Var "q") [p := True, q := False]
  -> And True (Not False)
   = And True True
   = And True

A proposition which evaluates to True for any binding is called a TAUTOLOGY.

Implement a function which checks if a proposition is a tautology.

tautology :: Prop -> Bool
> tautology (Or (Var "p") (Not $ Var "p"))
True
> tautology (Var "p")
False

NOTE:  Feel free to create more functions.
--}

import Data.Functor.Classes
import Data.List

data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop deriving Show


data PropNode a = AndNode a a | OrNode a a | NotNode a deriving (Show)

instance Functor PropNode where
  fmap f (NotNode x) = NotNode (f x)
  fmap f (AndNode x y) = AndNode (f x) (f y)
  fmap f (OrNode x y) = OrNode (f x) (f y)


data Free f a = Pure a | Free (f (Free f a))

instance (Show a, Show1 f) => Show (Free f a) where
  showsPrec p (Pure x) = showParen (p > 10)
    $ showString "Pure " . showsPrec 11 x
  showsPrec p (Free fx) = showParen (p > 10)
    $ showString "Free " . liftShowsPrec showsPrec showList (11) fx

-- instance (Show1 f, Show a) => Show (Free f a) where
--   showsPrec = showsPrec1

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap f <$> x

instance Functor f => Applicative (Free f) where
  pure = Pure
  -- (<*>)  :: Free f (a -> b) -> Free f a -> Free f b
  Pure x <*> f = x <$> f
  Free x <*> f = Free $ (<*> f) <$> x

instance Functor f => Monad (Free f) where
  -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Free x >>= f = Free $ (>>= f) <$> x

instance (Functor f, Foldable f) => Foldable (Free f) where
  -- foldr :: (a -> b -> b) -> b -> Free f a -> b
  foldr f b (Pure x) = f x b
  foldr f b (Free x) = foldr (\fa b' -> foldr f b' fa) b x

-- better expressed as:
-- mapFree :: (Functor f, Functor g) => (forall b. f b -> g b) -> Free f a -> Free g a
-- but that would require RankNTypes.
mapFree :: (Functor f, Functor g) => (f (Free g a) -> g (Free g a)) -> Free f a -> Free g a
mapFree f = foldFree (Free . f) Pure

foldFree :: (Functor f) => (f b -> b) -> (a -> b) -> Free f a -> b
foldFree f b (Pure x) = b x
foldFree f b (Free x) = f $ foldFree f b <$> x

type PropTree = Free PropNode String

toPropTree :: Prop -> PropTree
toPropTree (Var x) = Pure x
toPropTree (Not x) = Free $ NotNode (toPropTree x)
toPropTree (And x y) = Free $ AndNode (toPropTree x) (toPropTree y)
toPropTree (Or x y) = Free $ OrNode (toPropTree x) (toPropTree y)

type Var = String
-- | a snapshot of variable states
data VarState = VarTrue Var | VarFalse Var deriving (Eq, Show, Ord)
-- | forceTrue are possible states which can force the node to true,
-- forceFalse are possible states which will make the node false.
data VarResult = VarResult { forceTrue :: [VarState], forceFalse :: [VarState] } deriving (Eq, Show)

intersect' :: Ord a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) (y:ys) = case compare x y of
  EQ -> x : intersect' xs ys
  LT -> intersect' xs (y:ys)
  GT -> intersect' (x:xs) ys

union' :: Ord a => [a] -> [a] -> [a]
union' [] y = y
union' x [] = x
union' (x:xs) (y:ys) = case compare x y of
  EQ -> x : union' xs ys
  LT -> x : union' xs (y:ys)
  GT -> y : union' (x:xs) ys


toVarResult :: PropNode VarResult -> VarResult
toVarResult (NotNode (VarResult t f)) = VarResult f t
toVarResult (AndNode (VarResult t1 f1) (VarResult t2 f2)) = VarResult (intersect' t1 t2) (union' f1 f2)
toVarResult (OrNode (VarResult t1 f1) (VarResult t2 f2)) = VarResult (union' t1 t2) (intersect' f1 f2)

varToResult :: Var -> VarResult
varToResult x = VarResult [VarTrue x] [VarFalse x]

x = And (Var "p") (Not $ Var "q")

foldToVarState :: PropTree -> VarResult
foldToVarState = foldFree toVarResult varToResult


tautology :: Prop -> Bool
tautology = undefined