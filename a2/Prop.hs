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

import Data.List
import Data.Foldable

data Prop = Var String
          | And Prop Prop
          | Or Prop Prop
          | Not Prop
  deriving Show

data PropNode a = AndNode a a
                | OrNode a a
                | NotNode a
  deriving (Show)

instance Functor PropNode where
  fmap f (NotNode x) = NotNode (f x)
  fmap f (AndNode x y) = AndNode (f x) (f y)
  fmap f (OrNode x y) = OrNode (f x) (f y)

instance Foldable PropNode where
  foldr f b (NotNode x) = f x b
  foldr f b (AndNode x y) = f x $ f y b
  foldr f b (OrNode x y) = f x $ f y b

data Free f a = Pure a
              | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap f <$> x

instance (Functor f, Foldable f) => Foldable (Free f) where
  -- foldr :: (a -> b -> b) -> b -> Free f a -> b
  foldr f b (Pure x) = f x b
  foldr f b (Free x) = foldr (\fa b' -> foldr f b' fa) b x


foldFree :: (Functor f) => (f b -> b) -> (a -> b) -> Free f a -> b
foldFree f b (Pure x) = b x
foldFree f b (Free x) = f $ foldFree f b <$> x

newtype Variable = Variable String deriving (Eq, Show)

type PropTree = Free PropNode Variable

toPropTree :: Prop -> PropTree
toPropTree (Var x) = Pure $ Variable x
toPropTree (Not x) = Free $ NotNode (toPropTree x)
toPropTree (And x y) = Free $ AndNode (toPropTree x) (toPropTree y)
toPropTree (Or x y) = Free $ OrNode (toPropTree x) (toPropTree y)

varsInTree :: PropTree -> [Variable]
varsInTree = nub . toList

newtype TrueVars = TrueVars { trueVars :: [Variable] } deriving (Eq, Show)

powerset :: [a] -> [[a]]
powerset = foldr (\a b -> b ++ fmap (a:) b) [[]]

varCombinations :: [Variable] -> [TrueVars]
varCombinations = fmap TrueVars . powerset

setVar :: TrueVars -> Variable -> Bool
setVar = flip elem . trueVars

evalPropNode :: PropNode Bool -> Bool
evalPropNode (NotNode x) = not x
evalPropNode (OrNode x y) = x || y
evalPropNode (AndNode x y) = x && y

evalPropTree :: TrueVars -> PropTree -> Bool
evalPropTree = foldFree evalPropNode . setVar

falsifiable :: PropTree -> [TrueVars]
falsifiable t = filter evalToFalse . varCombinations . varsInTree $ t
  where
    evalToFalse :: TrueVars -> Bool
    evalToFalse = (== False) . flip evalPropTree t

tautology :: Prop -> Bool
tautology = null . falsifiable . toPropTree
