{-# LANGUAGE TemplateHaskell #-}
module Test.P.Monoid where

import           P.Monoid

import           Test.QuickCheck


prop_valueOrZero_true :: [Int] -> Property
prop_valueOrZero_true a = valueOrZero True a === a

prop_valueOrZero_false :: [Int] -> Property
prop_valueOrZero_false a = valueOrZero False a === []

prop_valueOrZero_empty :: Bool -> Property
prop_valueOrZero_empty b = valueOrZero b "" === zeroOrValue b ""

prop_zeroOrValue :: Bool -> [Int] -> Property
prop_zeroOrValue b a = a /= [] ==> valueOrZero b a /= zeroOrValue b a


return []
tests :: IO Bool
tests = $quickCheckAll
