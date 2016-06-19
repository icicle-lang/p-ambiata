{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.List (
    count
  , ordNub
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
--   /O(n log n)/ instead of /O(n²)/.
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

lastMaybe :: [a] -> Maybe a
lastMaybe = listToMaybe . reverse

-- | count the number of elements satisfying a predicate in a list
count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate
