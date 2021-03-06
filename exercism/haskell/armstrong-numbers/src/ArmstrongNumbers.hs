module ArmstrongNumbers (armstrong, digits) where

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digit : digits rest
    where
        digit = n `mod` 10
        rest = n `div` 10



armstrong :: Integral a => a -> Bool
armstrong n = (n ==) $ sum $ (^numDigits) <$> nDigits
    where
        nDigits = digits n
        numDigits = length nDigits
