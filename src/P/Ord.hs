module P.Ord (
    maxOn
  ) where

maxOn :: (Ord o) => (a -> o) -> a -> a -> a
maxOn f x y = if f x > f y then x else y
