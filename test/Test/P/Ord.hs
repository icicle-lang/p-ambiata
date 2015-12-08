{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.P.Ord where

import           P.Ord
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Test.QuickCheck


prop_maxOn_id a b = maxOn id a b === max a b

prop_sortOn_id as = sortOn snd as === (sortBy . comparing $ snd) as

return []
tests :: IO Bool
tests = $quickCheckAll
