{-# LANGUAGE TemplateHaskell #-}
module Test.P.List where

import           P.List

import qualified Data.List as L

import           Test.QuickCheck
import           Test.QuickCheck.Function

prop_ordNub :: (Ord a, Show a) => [a] -> Property
prop_ordNub a =
  ordNub a === L.nub a

prop_sortNub :: (Ord a, Show a) => [a] -> Property
prop_sortNub a =
  sortNub a === L.sort (L.nub a)


prop_lastMaybe :: (Eq a, Show a) => a -> [a] -> Property
prop_lastMaybe a l =
  lastMaybe (l ++ [a]) === Just a

prop_lastMaybe_empty :: Property
prop_lastMaybe_empty =
  lastMaybe [] === (Nothing :: Maybe ())

prop_count :: [Int] -> Fun Int Bool -> Property
prop_count list (Fun _ predicate) =
  count predicate list === length (filter predicate list)

prop_safe_index_is_safe :: Int -> Property
prop_safe_index_is_safe idx =
  let xs = [] :: [Int] in
  xs `safeIndex` idx === Nothing

prop_safe_index_correct :: Positive (Small Int) -> Property
prop_safe_index_correct (Positive (Small idx)) =
  let xs = [0..] :: [Int] in
  xs `safeIndex` idx === Just (xs L.!! idx)

return []
tests :: IO Bool
tests = $quickCheckAll
