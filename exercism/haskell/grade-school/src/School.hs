module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import           Data.Foldable (toList)

type School = M.Map Int (S.Set String)

add :: Int -> String -> School -> School
add n = M.insertWith S.union n . S.singleton

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade n = toList . M.findWithDefault S.empty n

sorted :: School -> [(Int, [String])]
sorted = M.assocs . M.map toList
