{-# LANGUAGE TemplateHaskell #-}
module Test.P.List where

import           P.List

import qualified Data.List as L

import           Test.QuickCheck

prop_nub :: (Ord a, Eq a, Show a) => [a] -> Property
prop_nub a =
  (L.sort . ordNub) a === (L.sort . L.nub) a

prop_lastMaybe :: (Eq a, Show a) => a -> [a] -> Property
prop_lastMaybe a l =
  lastMaybe (l ++ [a]) === Just a

prop_lastMaybe_empty :: Property
prop_lastMaybe_empty =
  lastMaybe [] === (Nothing :: Maybe ())

return []
tests :: IO Bool
tests = $quickCheckAll
