{-# LANGUAGE TemplateHaskell #-}
module Test.P.Applicative where

import           P.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Control.Applicative


prop_valueOrEmpty_true :: Int -> Property
prop_valueOrEmpty_true a = valueOrEmpty True a === Just a

prop_valueOrEmpty_false :: Int -> Property
prop_valueOrEmpty_false a = valueOrEmpty False a === Nothing

prop_emptyOrValue :: Bool -> Int -> Bool
prop_emptyOrValue b a = (valueOrEmpty b a :: Maybe Int) /= emptyOrValue b a

prop_orEmpty :: Int -> Property
prop_orEmpty i =
  let s = Sum i
  in
     (orEmpty (Just s) === Just s) .&&.
     (orEmpty (Nothing :: Maybe (Sum Int)) === Just (Sum 0))

return []
tests :: IO Bool
tests = $quickCheckAll
