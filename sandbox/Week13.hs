{-# LANGUAGE ScopedTypeVariables #-}

module Week13 where

f5 :: forall a b c. ((a -> b -> c) -> a) -> (a -> c) -> c
f5 f g = g (f (const . g))

foo :: (a -> b) -> a -> c -> b
foo = (.) const
