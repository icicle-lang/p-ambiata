{-# LANGUAGE TemplateHaskell #-}
module P.FunctionTest where

import           P.Function

import           Test.QuickCheck


prop_plus1 :: Int -> Property
prop_plus1 n = (n > 0) ==>
  applyN n (+1) 0 === n

return []
tests :: IO Bool
tests = $quickCheckAll
