{-# LANGUAGE TemplateHaskell #-}
module Test.P.EitherT where

import           P.EitherT
import           Control.Monad.Trans.Either
import           Data.Functor.Identity

import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_firstEitherT_id :: String -> Int -> Property
prop_firstEitherT_id s n =
  let (l, r) = (left s :: EitherT String Identity Int, right n :: EitherT String Identity Int)
  in  firstEitherT id l === l .&&.
      firstEitherT id r === r

prop_firstEitherT_map :: (Fun String Char) -> String -> Property
prop_firstEitherT_map fun s =
  let f = apply fun
      l = left s :: EitherT String Identity Int
  in  firstEitherT f l === left (f s)

prop_secondEitherT_id :: String -> Int -> Property
prop_secondEitherT_id s n =
  let (l, r) = (left s :: EitherT String Identity Int, right n :: EitherT String Identity Int)
  in  secondEitherT id l === l .&&.
      secondEitherT id r === r

prop_secondEitherT_map :: (Fun Int Char) -> Int -> Property
prop_secondEitherT_map fun n =
  let f = apply fun
      r = right n :: EitherT String Identity Int
  in  secondEitherT f r === right (f n)


return []
tests :: IO Bool
tests = $quickCheckAll
