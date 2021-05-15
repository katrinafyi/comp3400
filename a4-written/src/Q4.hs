module Q4 where

import Prelude hiding (map, iterate)

-- | 4.
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

-- | 4.1. fmap (g . h) = fmap g . fmap h.
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)

{-

We want to prove fmap (g . h) = fmap g . fmap h. We will show this by proving
the eta-expanded version. Let t :: Tree a, then

  fmap (g . h) t = fmap g . fmap h $ t.

First the base case where t = Leaf x.

  LHS = fmap (g . h) (Leaf x)                 -- substituting t = Leaf x
      = Leaf $ (g . h) x                      -- apply fmap
      = Leaf $ (\y -> g (h y)) x              -- apply (.)
      = Leaf $ g (h x)                        -- apply lambda
      = fmap g (Leaf (h x))                   -- unapply fmap
      = fmap g (fmap h (Leaf x))              -- unapply fmap
      = (\y -> fmap g (fmap h y)) (Leaf x)    -- unapply lambda
      = fmap g . fmap h $ Leaf x              -- unapply (.)
      = RHS                                   -- as required

Now assume it holds for subtrees l, r :: Tree a, so

  fmap (g . h) l = fmap g . fmap h $ l
  fmap (g . h) r = fmap g . fmap h $ r

and we will show it holds for t = Node l x r. Here, we will start with RHS.

  RHS = fmap g . fmap h $ Node l x r                    -- let t = Node l x r
      = (\y -> fmap g (fmap h y)) (Node l x r)          -- apply (.)
      = fmap g (fmap h (Node l x r))                    -- apply lambda
      = fmap g $ Node (fmap h l) (h x) (fmap h r)       -- apply inner fmap
      = Node (fmap g (fmap h l)) (g (h x)) (fmap g (fmap h r))
                                                        -- apply outer fmap
      = Node (fmap g . fmap h $ l) (g . h $ x) (fmap g . fmap h $ r)
                                                        -- unapply (.)
      = Node (fmap (g . h) l) (g . h $ x) (fmap (g . h) r)
                                                        -- inductive hypothesis
      = fmap (g . h) (Node l x r)                       -- unapply fmap
      = LHS                                             -- as required

We have shown fmap (g . h) t = fmap g . fmap h $ t for both Leaf and Node cases
so using structural induction it holds for all trees. We conclude that

  fmap (g . h) = fmap g . fmap h

as required.

-}

-- | 4.2. Applicative Tree.

{-

There is a lawful Applicative instance for Tree satisfying the laws:

  [Identity]      pure id <*> v = v
  [Homomorphism]  pure f <*> pure x = pure (f x)
  [Interchange]   u <*> pure y = pure ($ y) <*> u
  [Composition]   u <*> (v <*> w) = ((pure (.) <*> u) <*> v) <*> w

It also satisfies the relation between Functor and Applicative:

  fmap g x = pure g <*> x.

-}

instance Applicative Tree where
  -- pure :: a -> Tree a
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
map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail

-- | 5.2.
iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id
-- eta reduced.