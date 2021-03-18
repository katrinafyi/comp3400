module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs =
  let
    len = length xs
    ints :: [Int]
    ints = read . (:[]) <$> xs
  in
   (\i -> take n $ drop i ints) <$> [0..len - n]
