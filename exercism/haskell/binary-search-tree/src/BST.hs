module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.Functor.Classes ( Show1(liftShowsPrec), showsPrec1, Eq1(liftEq) )
import Control.Applicative (liftA2)

data Two a = Two a a

instance Eq1 Two where
  liftEq eq a b = and (liftA2 eq a b)

instance Eq a => Eq (Two a) where
  (==) = liftEq (==)

instance Show1 Two where
  liftShowsPrec showP showL p fa = showParen (p > 10)
    $ showString "Two" . foldr (\a b -> showChar ' ' . showP 11 a . b) id fa

instance (Show a) => Show (Two a) where showsPrec = showsPrec1

instance Functor Two where
  fmap f (Two a b) = Two (f a) (f b)

instance Applicative Two where
  pure a = Two a a
  Two fa fb <*> Two a b = Two (fa a) (fb b)

instance Foldable Two where
  foldr f x (Two a b) = f a $ f b x

instance Traversable Two where
  -- sequenceA :: Applicative f => Two (f a) -> f (Two a)
  sequenceA (Two a b) = Two <$> a <*> b


data Free f a = Pure a | Free (f (Free f a))

instance (Eq1 f) => Eq1 (Free f) where
  liftEq eq (Free a) (Free b) = liftEq (liftEq eq) a b

instance (Eq1 f, Eq a) => Eq (Free f a) where
  (==) = liftEq (==)

instance (Show a, Show1 f) => Show (Free f a) where
  showsPrec p (Pure x) = showParen (p > 10)
    $ showString "Pure " . showsPrec 11 x
  showsPrec p (Free fx) = showParen (p > 10)
    $ showString "Free " . liftShowsPrec showsPrec showList 11 fx

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

newtype BST a = BST { unBST :: Free Two a } deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft tree = error "You need to implement this function."

bstRight :: BST a -> Maybe (BST a)
bstRight tree = error "You need to implement this function."

bstValue :: BST a -> Maybe a
bstValue tree = error "You need to implement this function."

empty :: BST a
empty = error "You need to implement this function."

fromList :: Ord a => [a] -> BST a
fromList xs = error "You need to implement this function."

insert :: Ord a => a -> BST a -> BST a
insert x tree = error "You need to implement this function."

singleton :: a -> BST a
singleton x = error "You need to implement this function."

toList :: BST a -> [a]
toList tree = error "You need to implement this function."
