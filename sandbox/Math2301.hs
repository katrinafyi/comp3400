module Math2301 where

import qualified Data.Map.Strict as Map
import Data.Map (Map)
newtype Permutation a = Permutation { unPerm :: Map a a }
  deriving (Eq, Show)

makePermutation :: Ord a => Map a a -> Permutation a
makePermutation = Permutation . Map.filterWithKey (/=)

o :: Permutation a -> Permutation a -> Permutation a
(Permutation g) `o` (Permutation f) = makePermutation $ Map.unionWithKey g f
    where u :: a -> a -> a -> a
          u k v1 v2 = Map.findWithDefault v1 k Map k a