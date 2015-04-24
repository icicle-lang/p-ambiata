{-# LANGUAGE TemplateHaskell #-}
module Test.P.Bool where

import           Control.Monad.Trans.Writer

import           P.Bool

import           Test.QuickCheck


prop_bool :: Bool -> Int -> Int -> Property
prop_bool p x y =
  (if p then x else y) === bool y x p

prop_whenM_positive :: Int -> Int -> Property
prop_whenM_positive x y =
  execWriter (whenM (w True [x]) (w () [y])) === [x, y]

prop_whenM_negative :: Int -> Int -> Property
prop_whenM_negative x y =
  execWriter (whenM (w False [x]) (w () [y])) === [x]

prop_unlessM_positive :: Int -> Int -> Property
prop_unlessM_positive x y =
  execWriter (unlessM (w True [x]) (w () [y]))=== [x]

prop_unlessM_negative :: Int -> Int -> Property
prop_unlessM_negative x y =
  execWriter (unlessM (w False [x]) (w () [y])) === [x, y]

prop_whenM_unlessM_exclusive :: Bool -> Int  -> Property
prop_whenM_unlessM_exclusive p x =
  (execWriter $ do
     unlessM (w p []) (w () [x])
     whenM (w p []) (w () [x])) === [x]

prop_ifM_positive :: Int -> Int -> Int -> Int -> Int -> Property
prop_ifM_positive x y z a b =
  runWriter (ifM (w True [x]) (w a [y]) (w b [z])) === (a, [x, y])

prop_ifM_negative :: Int -> Int -> Int -> Int -> Int -> Property
prop_ifM_negative x y z a b =
  runWriter (ifM (w False [x]) (w a [y]) (w b [z])) === (b, [x, z])

prop_ifM_exclusive :: Bool -> Int -> Int -> Int -> Property
prop_ifM_exclusive p x y a =
  runWriter (ifM (w p [x]) (w a [y]) (w a [y])) === (a, [x, y])

w :: a -> w -> Writer w a
w = curry writer

return []
tests :: IO Bool
tests = $quickCheckAll
