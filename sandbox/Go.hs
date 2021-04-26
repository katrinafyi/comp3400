{-# language ScopedTypeVariables #-}


module Go where

data Error a = InvalidInputBase
             | InvalidOutputBase
             | InvalidDigit a
  deriving (Show, Eq)

toBase :: forall a. Integral a => a -> a -> Either (Error a) [a]
toBase outputBase decNum
    | decNum < 0     = undefined
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise      = Right $ go [] decNum
    where
        go :: Integral a => [a] -> a -> [a]
        go acc n =
            let
                (quotient, remainder) = divMod n outputBase
                acc' = remainder : acc
            in
                if quotient == 0
                    then acc'
                    else go acc' quotient

foo :: Integral a => a -> a -> a
foo x y = go x
    where go :: Integral a => a -> a
          go a = a + y
