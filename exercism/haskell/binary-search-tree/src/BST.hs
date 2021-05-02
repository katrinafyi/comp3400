module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList) where

import Data.Foldable (foldl')

type BST = Tree

data Tree a = Nil
            | Node (BST a) a (BST a)
  deriving (Show, Eq)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Nil = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Nil = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Nil = Nothing
bstValue (Node _ x _) = Just x

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Nil = singleton x
insert x (Node l y r)
  | x <= y = Node l' y r
  | otherwise = Node l y r'
  where
    l' = insert x l
    r' = insert x r

singleton :: a -> BST a
singleton x = Node Nil x Nil

toList :: BST a -> [a]
toList Nil = []
toList (Node l x r) = toList l ++ [x] ++ toList r
