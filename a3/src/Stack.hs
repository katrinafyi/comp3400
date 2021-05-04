module Stack (stackFoldl, stackFoldr, stackZip, stackMap) where

{-
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

A Stack is a data structure typically used for first in, last out (FILO)

Consider the following data constructor...
    data Stack a = Empty | Stack (Stack a) a deriving Show

You task is to implement some of the typical higher-order-function for this
list.

=======
EXAMPLE
=======

> as = Empty
> bs = Stack as 1
> cs = Stack bs 2
> xs = Stack cs 3
> ys = Stack xs 4
> zs = Stack ys 5

> stackFoldl (*) 6 zs
720

> stackFoldr (*) 6 zs
720

> stackMap (2*) zs
Stack (Stack (Stack (Stack (Stack Empty 2) 4) 6) 8) 10

> ks = stackMap (2*) zs
> stackZip xs ks-- as with built-in zip, truncate the longer stack.
Stack (Stack (Stack Empty (1,2)) (2,4)) (3,6)

NOTE:  Passing these tests ONLY is DEFINITELY insufficient to conclude your code
is correct.  For instance, the folding examples use a commutative operator and
thus do not actually distinguishing between left and right folding.

Part of this assessment is producing tests for your own code.  Indeed your
written submission is asking for some tests.

-}

data Stack a = Empty | Stack (Stack a) a deriving Eq

instance Show a => Show (Stack a) where
  showsPrec _ Empty = showString "Empty"
  showsPrec p (Stack xs x) = showParen (p >= 5)
    $ showsPrec 5 x . showString " .: " . showsPrec 5 xs


infixr 5 .:
(.:) :: a -> Stack a -> Stack a
(.:) = flip Stack

stackFoldl :: (b -> a -> b) -> b -> Stack a -> b
stackFoldl _ b Empty = b
stackFoldl f b (Stack xs x) = stackFoldl f (f b x) xs

stackFoldr :: (a -> b -> b) -> b -> Stack a -> b
stackFoldr _ b Empty = b
stackFoldr f b (Stack xs x) =  x `f` stackFoldr f b xs

stackZip :: Stack a -> Stack b -> Stack (a,b)
stackZip (Stack xs x) (Stack ys y) = (x,y) .: stackZip xs ys
stackZip _ _ = Empty

stackMap :: (a -> b) -> Stack a -> Stack b
stackMap f = stackFoldr ((.:) . f) Empty
