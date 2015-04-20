{-# LANGUAGE TemplateHaskell #-}
module P.FoldableTest where

import           Control.Monad.State

import           P.Foldable

import           Test.QuickCheck


prop_findMapM_first :: Int -> [Int] -> Property
prop_findMapM_first x xs =
  runState (findMapM found (x : xs)) 0 === (Just $ x * 2, 1)

prop_findMapM_last :: Int -> [Int] -> Property
prop_findMapM_last x xs = not (elem x xs) ==>
  let f z = if z == x then found z else notfound
  in runState (findMapM f (xs ++ [x])) 0 === (Just $ x * 2, length xs + 1)

prop_findMapM_effects :: [Int] -> Property
prop_findMapM_effects xs =
  runState (findMapM (const notfound) xs) 0 === (Nothing, length xs)


found :: Int -> State Int (Maybe Int)
found z = StateT $ \n -> return (Just $ z * 2, n + 1)

notfound :: State Int (Maybe Int)
notfound = StateT $ \n -> return (Nothing, n + 1)


return []
tests :: IO Bool
tests = $quickCheckAll
