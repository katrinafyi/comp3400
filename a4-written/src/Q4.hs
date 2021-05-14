module Q4 where

-- | 4.
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)

instance Applicative Tree where
  pure x = let node = Node node x node in node
  -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> Leaf x = Leaf (f x)
  Leaf f <*> Node _ x _ = Leaf (f x)
  Node _ f _ <*> Leaf x = Leaf (f x)
  Node fl f fr <*> Node xl x xr = Node (fl <*> xl) (f x) (fr <*> xr)


unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

-- | 5.1.
map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

-- | 5.2.
iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id