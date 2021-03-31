module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map.Strict as M
import           Data.Maybe
import           Control.Applicative
import           Data.Functor.Identity


data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- | A Garden is a Map of student names (as String) to a list of their Plants.
newtype Garden = Garden (M.Map String [Plant]) deriving (Eq, Show)

-- | Converts a character to the Plant if possible.
toPlant :: Char -> Maybe Plant
toPlant 'C' = Just Clover
toPlant 'G' = Just Grass
toPlant 'R' = Just Radishes
toPlant 'V' = Just Violets
toPlant _ = Nothing

-- | Given a list, returns a list of list where each list has n elements.
-- The final list in the returned list may have less than n if n does not
-- evenly divide the input list's length.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- | Applies a fold down the columns of a 2D list of lists.
-- Folds through the outer foldable f, each step applying the given function
-- through the applicative g.
-- The applicative instance describes how the current level will be combined
-- with the result from folding the remainder.
-- The foldable describes the pattern of the fold.
foldAp :: (Foldable f, Applicative g) => (a -> b -> b) -> b -> f (g a) -> g b
foldAp f b = foldr (liftA2 f) (pure b)

-- We can do interesting things by playing with the Applicative instance.

-- We can recover a normal fold by choosing Identity as the Applicative.
notFoldr :: (Functor f, Foldable f) => (a -> b -> b) -> b -> f a -> b
notFoldr f b = runIdentity . foldAp f b . fmap Identity

-- We can recover our original foldDown by converting lists to ZipList, since
-- zipWith f = liftA2 f where liftA2 is from the ZipList instance.
foldDown :: (Functor f, Foldable f) => (a -> b -> b) -> b -> f [a] -> [b]
foldDown f b = getZipList . foldAp f b . fmap ZipList

-- | Transposes a 2D list (for fun).
transpose :: [[a]] -> [[a]]
transpose = foldDown (:) []

-- | Given a list of students and a string representing plants, build the Garden.
garden :: [String] -> String -> Garden
garden s p = Garden (M.fromList pairs)
  where
    -- split plants into rows and convert to Plant.
    rows = fmap toPlant <$> lines p
    -- within each row, break into chunks of 2 plants each.
    chunked = chunks 2 <$> rows
    -- collect each student's plants down the colums by folding.
    squares = foldDown (++) [] chunked
    -- generate a list of (student, plants) tuples, then remove Nothing from
    -- list of plants. we filter Nothing last so invalid plants early no do not
    -- break the positioning of plants later in the same row.
    pairs = fmap catMaybes <$> zip s squares

-- | Returns the student's plants in the given garden.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants s (Garden g) = M.findWithDefault [] s g
