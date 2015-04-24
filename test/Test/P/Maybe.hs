{-# LANGUAGE TemplateHaskell #-}
module Test.P.Maybe where

import           P.Maybe

import           Test.QuickCheck


prop_fromMaybeM_identity :: (Eq a, Show a) => Maybe a -> Property
prop_fromMaybeM_identity a = fromMaybeM Nothing a === a

prop_fromMaybeM_just :: (Eq a, Show a) => a -> Property
prop_fromMaybeM_just a = fromMaybeM (Left ()) (Just a) === Right a

prop_fromMaybeM_nothing :: (Eq a, Show a) => a -> Property
prop_fromMaybeM_nothing a = fromMaybeM (Left a) (Nothing :: Maybe ()) === Left a

return []
tests :: IO Bool
tests = $quickCheckAll
