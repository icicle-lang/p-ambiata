{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.List (
    count
  , ordNub
  , sortNub
  , lastMaybe
  ) where

import           Data.Bool (Bool)
import           Data.Function ((.))
import           Data.Int (Int)
import           Data.List (reverse, length, filter)
import           Data.List.NonEmpty ()
import           Data.Maybe (Maybe, listToMaybe)
import           Data.Ord (Ord)
import qualified Data.Set as Set

-- | /O(n log n)/ Remove duplicate elements from a list.
--
--   Unlike 'Data.List.nub', this version requires 'Ord' and runs in
--   /O(n log n)/ instead of /O(n²)/. Like 'Data.List.nub' the output
--   order is identical to the input order.
--   See 'sortNub' for `nub` behaviour with sorted output.
--
--   > ordNub "foo bar baz" == "fo barz"
--   > ordNub [3,2,1,2,1] == [3,2,1]
--   > List.take 3 (ordNub [4,5,6,undefined]) == [4,5,6]
--   > ordNub xs == List.nub xs
--
ordNub :: Ord a => [a] -> [a]
ordNub =
  let
    loop seen = \case
      [] ->
        []
      x : xs ->
        if Set.member x seen then
          loop seen xs
        else
          x : loop (Set.insert x seen) xs
  in
    loop Set.empty

-- | /O(n log n)/ Sort and remove duplicate elements from a list.
--
--   Unlike 'Data.List.nub', this version requires 'Ord' and runs in
--   /O(n log n)/ instead of /O(n²)/. The output list is returned in
--   sorted order.
--
--   > sortNub [3,2,1,2,1] == [1,2,3]
--   > sortNub xs == List.sort (List.nub xs)
--
sortNub :: Ord a => [a] -> [a]
sortNub = Set.toList . Set.fromList


lastMaybe :: [a] -> Maybe a
lastMaybe = listToMaybe . reverse

-- | count the number of elements satisfying a predicate in a list
count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate
