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
import Data.Foldable
import Data.Maybe

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

instance (Show a, Show1 f) => Show (Free f a) where
  showsPrec p (Pure x) = showParen (p > 10)
    $ showString "Pure " . showsPrec 11 x
  showsPrec p (Free fx) = showParen (p > 10)
    $ showString "Free " . liftShowsPrec showsPrec showList (11) fx

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
mapFree :: (Functor f, Functor g)
        => (f (Free g a) -> g (Free g a))
        -> Free f a
        -> Free g a
mapFree f = foldFree (Free . f) Pure

foldFree :: (Functor f) => (f b -> b) -> (a -> b) -> Free f a -> b
foldFree f b (Pure x) = b x
foldFree f b (Free x) = f $ foldFree f b <$> x

newtype Variable = Variable String
  deriving (Eq, Show)

type PropTree = Free PropNode Variable

toPropTree :: Prop -> PropTree
toPropTree (Var x) = Pure $ Variable x
toPropTree (Not x) = Free $ NotNode (toPropTree x)
toPropTree (And x y) = Free $ AndNode (toPropTree x) (toPropTree y)
toPropTree (Or x y) = Free $ OrNode (toPropTree x) (toPropTree y)

x = And (Var "p") (Not $ Var "q")

t = Or (Var "p") (Not $ Var "p")

c = And (Var "p") (Not $ Var "p")

x2 = Or (And (Var "p") (Not $ Var "q")) (And (Not $ Var "p") (Var "q"))

varsInTree :: PropTree -> [Variable]
varsInTree = nub . toList

v = varsInTree $ toPropTree x2

newtype VarState = VarState { varTuple :: (Variable, Bool) }
  deriving (Eq, Show)

varStates :: Variable -> [VarState]
varStates v = [VarState (v, False), VarState (v, True)]

varCombinations :: [Variable] -> [[VarState]]
varCombinations = traverse varStates

setVar :: [VarState] -> Variable -> Bool
setVar vars = fromJust . flip lookup (fmap varTuple vars)
-- should never throw in normal execution, but can easily throw if misused.

evalPropNode :: PropNode Bool -> Bool
evalPropNode (NotNode x) = not x
evalPropNode (OrNode x y) = x || y
evalPropNode (AndNode x y) = x && y

evalPropTree :: [VarState] -> PropTree -> Bool
evalPropTree = foldFree evalPropNode . setVar

falsifiable :: PropTree -> [[VarState]]
falsifiable t = filter evalToFalse . varCombinations . varsInTree $ t
  where
    evalToFalse :: [VarState] -> Bool
    evalToFalse = (== False) . flip evalPropTree t

tautology :: Prop -> Bool
tautology = null . falsifiable . toPropTree
