module Potato where

--- do not change anything above this line ---

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

During a game of "hot potato" a token (a potato) is passed amongst a group of
people until some timer goes off at which point the game ends.

Suppose a game of "hot potato" is played among people with unique Integer
identifiers. So, for instance, a game with the following moves
(0, 1) -- Person 0 passes potato to Person 1
(1, 2) -- Person 1 passes potato to Person 2
(2, 3) -- Person 2 passes potato to Person 3
(3, 4) -- Person 3 passes potato to Person 4
(4, 5) -- Person 4 passes potato to Person 5 and game ends.
can be ENCODED as
[0, 1, 2, 3, 4, 5]

You can also have more sophisticated games where the potato is passed back
to people who have already had the potato:
(0, 2)
(2, 0)
(0, 1)
(1, 2)
(2, 1)
which gets ENCODED as
[0, 2, 0, 1, 2, 1]

ASSUME: Person 0 ALWAYS initially has the potato and write a function
    encode :: [(Int, Int)] -> Maybe [Int]
that given a list of passes made during a game, returns the encoded game.
    > encode [(0, 2), (2, 0), (0, 1), (1, 2), (2, 0)]
    Just [0, 2, 0, 1, 2, 0]
otherwise, if the tuples correspond to an impossible game, return Nothing.
    > encode [(0, 2), (2, 0), (1, 2)]
    Nothing

Q:  Why is this hard?
A:  The list of tuples will not be given in order.

    > encode [(4,5),(1,2),(2,3),(0,1),(3,4)]
    Just [0, 1, 2, 3, 4, 5]

    > encode [(0, 1), (0, 2), (1, 2), (2, 0), (2, 1)]
    Just [0, 2, 0, 1, 2, 1]

When there are MULTIPLE CORRECT ENCODINGS.  Return the encoding that is LEAST
when converted into a base ten number.

    > encode [(0, 1), (0, 2), (1, 2), (2, 0), (2, 1)]
    Just [0, 1, 2, 0, 2, 1]
    -- even though [0, 2, 1, 2, 0, 1] is also a valid game, 12021 < 21201 and
    thus the former is returned.
--}

import           Data.List (delete)
import           Data.Maybe (listToMaybe)
import           Control.Monad (guard)
import           Data.List.NonEmpty (nonEmpty)

-- | Returns true if the given dominos could form an adjacent pair.
isPair :: Eq a => (b, a) -> (a, c) -> Bool
isPair (_, x) (y, _) = x == y

-- | Returns a list of tuples where the first element is a value and the second
-- is the remainder of the list without that element.
select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : map (fmap (x:)) (select xs)


-- | Returns all chains which could appear following the given first value.
-- The first value does not appear in any of the lists.
chainFrom :: (a -> a -> Bool) -> a -> [a] -> [[a]]
chainFrom _ _ [] = pure []
chainFrom p x xs = do
  (y, ys) <- filter (p x . fst) $ select xs
  (y:) <$> chainFrom p y ys

chainFromAll :: (a -> a -> Bool) -> [a] -> [[a]]
chainFromAll _ [] = pure []
chainFromAll p xs = do
  (y, ys) <- select xs
  (y:) <$> chainFrom p y ys

encode :: [(Int, Int)] -> Maybe [Int]
encode xs = fmap minimum . nonEmpty $ do
  game <- chainFromAll isPair xs
  (x, _) <- take 1 game
  guard $ x == 0
  pure $ x : fmap snd game
