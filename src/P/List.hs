{-# LANGUAGE NoImplicitPrelude #-}
module P.List (
    ordNub
  , lastMaybe
  ) where

import Data.Eq
import Data.Ord
import Data.Function ((.))
import Data.Functor (fmap)
import Data.List hiding (head, group)
import Data.List.NonEmpty (head, group)
import Data.Maybe (Maybe, listToMaybe)

-- |
-- Like `nub` from Prelude, but adds an `Ord` constraint to boost efficiency a little bit,
-- though it could be improved even more with a Set or hashmap (with a `Hashable` constraint instead)
-- WARNING: This is not stable due to sort
ordNub :: (Ord a, Eq a) => [a] -> [a]
ordNub = fmap head . group . sort

lastMaybe :: [a] -> Maybe a
lastMaybe = listToMaybe . reverse
