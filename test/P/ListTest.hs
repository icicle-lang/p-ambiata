{-# LANGUAGE TemplateHaskell #-}
module P.ListTest where

import           P.List

import qualified Data.List as L

import           Test.QuickCheck

prop_nub :: (Ord a, Eq a, Show a) => [a] -> Property
prop_nub a =
  (L.sort . ordNub) a === (L.sort . L.nub) a

return []
tests :: IO Bool
tests = $quickCheckAll
