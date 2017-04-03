{-# LANGUAGE TemplateHaskell #-}
module Test.P.List where

import           P.List

import           Data.Function (on)
import qualified Data.List as L
import           Data.Ord (comparing)

import           Test.QuickCheck
import           Test.QuickCheck.Function

prop_ordNub :: (Ord a, Show a) => [a] -> Property
prop_ordNub a =
  ordNub a === L.nub a

prop_ordNubBy :: (Ord a, Show a) => [[a]] -> Property
prop_ordNubBy a =
  ordNubBy (comparing length) a === L.nubBy ((==) `on` length) a

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

return []
tests :: IO Bool
tests = $quickCheckAll
