{-# LANGUAGE TemplateHaskell #-}

import Stack
import Test.QuickCheck
import Test.Hspec
import Data.Char(ord)
import Test.Hspec.QuickCheck (prop)

-- | Infix cons operator for Stack. Analagous to (:) for lists.
infixr 4 .:
(.:) :: a -> Stack a -> Stack a
(.:) = flip Stack

-- | Converts the given stack to a list.
toList :: Stack a -> [a]
toList = stackFoldr (:) []

-- | Converts the given list to a stack.
fromList :: [a] -> Stack a
fromList = foldr (flip Stack) Empty


-- left fold with flip (.:) and Empty should reverse the stack.
prop_stackFoldl1 = \xs -> toList (stackFoldl (flip (.:)) Empty (fromList xs))
    == reverse (xs :: [Int])
-- testing folding with non-commutative operator should behave like foldl.
prop_stackFoldl2 = \xs -> stackFoldl (++) "a" (fromList xs)
    == foldl (++) "a" (xs :: [String])


-- right fold with (.:) and Empty should be identity.
prop_stackFoldr1 = \xs -> toList (stackFoldr (.:) Empty (fromList xs))
    == (xs :: [Int])
-- right fold with non-commutative operator should behave like foldr.
prop_stackFoldr2 = \xs -> stackFoldr (++) "a" (fromList xs)
    == foldr (++) "a" (xs :: [String])

-- stackZip behaves just like zip.
prop_stackZip1 = \xs ys -> toList (stackZip (fromList xs) (fromList ys))
    == zip (xs :: [Char]) (ys :: [Int])
-- stackZip with one Empty argument is Empty.
prop_stackZip2 = \xs -> case stackZip (fromList xs) Empty of
    Empty -> True
    _ -> False

-- mapping with identity should not change the stack.
prop_stackMap1 = \xs -> toList (stackMap id (fromList xs))
    == (xs :: [Int])
-- mapping behaves identically to map for lists.
prop_stackMap2 = \xs -> toList (stackMap ord (fromList xs))
    == map ord (xs :: [Char])

return []
runPropTests = $quickCheckAll



main :: IO ()
main = do
  -- runPropTests
  hspec $ do
    describe "Stack" $ do
      it "stackFoldl" $ do
        stackFoldl (+) 101 Empty `shouldBe` 101
        stackFoldl (flip (:)) "0" (Stack (Stack Empty 'A') 'B') `shouldBe` "AB0"
      it "stackFoldr" $ do
        stackFoldr (+) 102 Empty `shouldBe` 102
        stackFoldr (:) "0" (Stack (Stack Empty 'A') 'B') `shouldBe` "BA0"
      it "stackZip" $ do
        stackZip Empty (Stack (Stack Empty 1) 2) `shouldBe` (Empty :: Stack (Int, Int))
        stackZip (Stack (Stack (Stack Empty 'a') 'b') 'c') (Stack (Stack Empty 1) 2)
            `shouldBe` Stack (Stack Empty ('b', 1)) ('c', 2)
      it "stackMap" $ do
        stackMap (+100) Empty `shouldBe` Empty
        stackMap (\x -> x^2 + 2) (Stack (Stack Empty 1) 2) `shouldBe` (Stack (Stack Empty 3) 6)
