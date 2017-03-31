{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.List (
    count
  , ordNub
  , ordNubBy
  , sortNub
  , lastMaybe
  ) where

import           Data.Bool (Bool)
import           Data.Eq (Eq(..))
import           Data.Function ((.))
import           Data.Int (Int)
import           Data.List (reverse, length, filter)
import           Data.List.NonEmpty ()
import           Data.Maybe (Maybe, listToMaybe)
import           Data.Ord (Ord(..), Ordering(..))
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
  ordNubBy compare

-- | /O(n log n)/ Behaves exactly like 'ordNub' except it uses a user-supplied
--  comparison function.
--
--  > ordNubBy (comparing length) ["foo","bar","quux"] == ["foo","quux"]
--  > ordNubBy (comparing fst) [("foo", 10),("foo", 20),("bar", 30)] == [("foo", 10),("bar", 30)]
--
ordNubBy :: (a -> a -> Ordering) -> [a] -> [a]
ordNubBy f =
  let
    loop seen = \case
      [] ->
        []
      x : xs ->
        let
          y =
            UserOrd f x
        in
          if Set.member y seen then
            loop seen xs
          else
            x : loop (Set.insert y seen) xs
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

--
-- Some machinery so we can use Data.Set with a custom comparator.
--

data UserOrd a =
  UserOrd (a -> a -> Ordering) a

instance Eq (UserOrd a) where
  (==) (UserOrd f x) (UserOrd _ y) =
    f x y == EQ

instance Ord (UserOrd a) where
  compare (UserOrd f x) (UserOrd _ y) =
    f x y
