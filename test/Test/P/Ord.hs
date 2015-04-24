{-# LANGUAGE TemplateHaskell #-}
module Test.P.Ord where

import           P.Ord

import           Test.QuickCheck


prop_maxOn_id :: (Ord a, Show a) => a -> a -> Property
prop_maxOn_id a b = maxOn id a b === max a b


return []
tests :: IO Bool
tests = $quickCheckAll
