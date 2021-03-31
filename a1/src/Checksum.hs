module Checksum (checkSum) where

{--
A valid credit card number satisfies a property known as a CHECK SUM.

The check sum for credit cards is as follows.  If n :: Integer satisfies
1/  n is a 16 digit number XXXX XXXX XXXX XXXX (the blanks aren't really there).

Designating odd "O" and even "E" positions in n by
  OEOE OEOE OEOE OEOE
then
2/  If the (sum of the odd digits) + 2*(sum of even digits) is divisilbe by 10
    then the card is valued.

For example,
OEOE OEOE OEOE OEOE
1234 5678 9012 3452
(1+3+5+7+9+1+3+5) + 2*(2+4+6+8+0+2+4+2)
= 90
which is divisible by 10 and therefore valid.

1111 1111 1111 1111
8 + 2*8
= 24
which is NOT divisible by 10 and therefore NOT valid.

=======
EXAMPLE
=======
> checkSum 1234567890123453
True
> checkSum 1111111111111111
False
--}

-- | returns a list of digits in the given number, most significant first.
-- leading 0s are not considered part of the number, so [] is returned for 0.
digits :: Integral a => a -> [a]
digits = reverse . go
  where
    go :: Integral a => a -> [a]
    go n
      | n <= 0 = []
      | otherwise = digit : go rest
      where
        digit = n `mod` 10
        rest = n `div` 10

-- | Filters the given list by the given predicate on indices.
filterIndices :: (Int -> Bool) -> [a] -> [a]
filterIndices f = fmap snd . filter (f . fst) . zip [1..]

-- | verifies the checksum property for the given list of digits.
checkDigits :: [Integer] -> Bool
checkDigits ds =
  let
    odds = filterIndices odd ds
    evens = filterIndices even ds
  in
    length ds == 16 && (sum odds + 2 * sum evens) `mod` 10 == 0

-- | verifies the checksum property for the given number.
checkSum :: Integer -> Bool
checkSum = checkDigits . digits
-- TODO: double check behaviour with 0.

-- main :: IO ()
-- main = do
--     print $ checkSum 1234567890123453
--     print $ checkSum 1111111111111111
--     print $ checkSum 0000000000000000
