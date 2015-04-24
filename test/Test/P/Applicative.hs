{-# LANGUAGE TemplateHaskell #-}
module Test.P.Applicative where

import           P.Applicative

import           Test.QuickCheck


prop_valueOrEmpty_true :: Int -> Property
prop_valueOrEmpty_true a = valueOrEmpty True a === Just a

prop_valueOrEmpty_false :: Int -> Property
prop_valueOrEmpty_false a = valueOrEmpty False a === Nothing

prop_emptyOrValue :: Bool -> Int -> Bool
prop_emptyOrValue b a = (valueOrEmpty b a :: Maybe Int) /= emptyOrValue b a


return []
tests :: IO Bool
tests = $quickCheckAll
