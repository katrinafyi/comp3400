module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Empty
                  | a `Cons` (LinkedList a)
  deriving (Eq, Show)

infixr 4 `Cons`

datum :: LinkedList a -> a
datum (x `Cons` _) = x
datum Empty = error "datum of empty list"

fromList :: [a] -> LinkedList a
fromList = foldr Cons Empty

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next (_ `Cons` xs) = xs
next Empty = error "next of empty list"

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Empty
  where
    go :: LinkedList a -> LinkedList a -> LinkedList a
    go acc Empty = acc
    go acc (x `Cons` xs) = go (x `Cons` acc) xs

toList :: LinkedList a -> [a]
toList Empty = []
toList (x `Cons` xs) = x:toList xs
