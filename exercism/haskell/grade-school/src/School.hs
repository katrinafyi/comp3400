module School (School, add, empty, grade, sorted) where

import           Data.Foldable (toList)
import           Data.List (sortOn, sort)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

data Student = Student { studentName :: String, studentGrade :: Int}
  deriving (Eq, Show)

newtype School = School { roster :: [Student] }
  deriving (Eq, Show)

add :: Int -> String -> School -> School
add g n s = School $ Student n g : roster s

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade g = sort . fmap studentName . filter ((== g) . studentGrade) . roster

flattenWithKey :: (a -> k) -> (a -> v) -> NonEmpty a -> (k, NonEmpty v)
flattenWithKey fk fv as = (fk $ N.head as, fmap fv as)

groupWithKey
  :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, NonEmpty v)]
groupWithKey fk fv =
  fmap (flattenWithKey fk fv) . N.groupBy (\a b -> fk a == fk b) . sortOn fk

sorted :: School -> [(Int, [String])]
sorted s = fmap (sort . toList) <$> grouped
  where grouped = groupWithKey studentGrade studentName (roster s)
