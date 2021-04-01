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
import Control.Applicative

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
data VarState = VarTrue { var :: Var } | VarFalse { var :: Var }  deriving (Eq, Show, Ord)
-- | surelyTrue are possible states which can force the node to true,
-- surelyFalse are possible states which will make the node false.
data VarResult = VarResult { surelyTrue :: [VarState], surelyFalse :: [VarState] } deriving (Eq, Show)


data Literal a = Literal a | Negation a deriving (Eq, Show)
newtype Conjunct a = Conjunct [a] deriving (Eq, Show)
newtype Disjunct a = Disjunct [a] deriving (Eq, Show)

instance Functor Conjunct where
  fmap f (Conjunct x) = Conjunct (fmap f x)
instance Applicative Conjunct where
  pure x = Conjunct $ pure x
  liftA2 f (Conjunct x) (Conjunct y) = Conjunct $ liftA2 f x y

instance Functor Disjunct where
  fmap f (Disjunct x) = Disjunct (fmap f x)
instance Applicative Disjunct where
  pure x = Disjunct $ pure x
  liftA2 f (Disjunct x) (Disjunct y) = Disjunct $ liftA2 f x y

type CNF = Conjunct (Disjunct (Literal String))

-- https://www.cs.jhu.edu/~jason/tutorials/convert-to-CNF.html

treeToCNF :: PropNode CNF -> CNF
treeToCNF (NotNode (Conjunct [Disjunct [Literal x]])) = Conjunct [Disjunct [Negation x]]
treeToCNF (NotNode (Conjunct [Disjunct [Negation x]])) = Conjunct [Disjunct [Literal x]]
treeToCNF (NotNode (Conjunct (c:cs))) = treeToCNF (OrNode ())
treeToCNF (AndNode (Conjunct c1) (Conjunct c2)) = Conjunct (c1 ++ c2)
treeToCNF (OrNode (Conjunct c1) (Conjunct c2)) = mconcat . sequenceA $ Conjunct [c1, c2]

varToCNF :: Var -> CNF
varToCNF x = Conjunct [Disjunct [Literal x]]

foldToCNF :: PropTree -> CNF
foldToCNF = foldFree treeToCNF varToCNF


x = And (Var "p") (Not $ Var "q")
t = Or (Var "p") (Not $ Var "p")

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- tautology :: Prop -> Bool
-- tautology = hasDuplicates . fmap var . surelyTrue . foldToVarState . toPropTree