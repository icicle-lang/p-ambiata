module P.Ord (
    maxOn
  , sortOn
  ) where

import         Data.List (sortBy)
import         Data.Ord (comparing)

maxOn :: (Ord o) => (a -> o) -> a -> a -> a
maxOn f x y = if f x > f y then x else y

sortOn :: (Ord o) => (a -> o) -> [a] -> [a]
sortOn = sortBy . comparing
