{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.P.Foldable where

import           Control.Applicative (pure)
import           Control.Monad.State (StateT(..), State, runState)

import           Data.Bool (Bool)
import           Data.Either (Either(..))
import           Data.Function (($), const)
import           Data.Int (Int)
import qualified Data.List as List
import           Data.Maybe (Maybe(..))

import           P.Foldable

import           Prelude (Eq(..), Num(..))

import           System.IO (IO)

import           Test.QuickCheck


prop_findMapM_first :: Int -> [Int] -> Property
prop_findMapM_first x xs =
  runState (findMapM found (x : xs)) 0 === (Just $ x * 2, 1)

prop_findMapM_last :: Int -> [Int] -> Property
prop_findMapM_last x xs = List.notElem x xs ==>
  let f z = if z == x then found z else notfound
  in runState (findMapM f (xs List.++ [x])) 0 === (Just $ x * 2, List.length xs + 1)

prop_findMapM_effects :: [Int] -> Property
prop_findMapM_effects xs =
  runState (findMapM (const notfound) xs) 0 === (Nothing, List.length xs)


prop_head_either_left :: Int -> Property
prop_head_either_left r = head (Left r) === (Nothing :: Maybe ())

prop_head_either_right :: Int -> Property
prop_head_either_right r = head (Right r) === pure r

prop_head_list_nonempty :: Int -> [Int] -> Property
prop_head_list_nonempty x xs = head (x:xs) === pure x

prop_head_list_empty :: Property
prop_head_list_empty = head ([] :: [Int]) === Nothing

found :: Int -> State Int (Maybe Int)
found z = StateT $ \n -> pure (Just $ z * 2, n + 1)

notfound :: State Int (Maybe Int)
notfound = StateT $ \n -> pure (Nothing, n + 1)


pure []
tests :: IO Bool
tests = $quickCheckAll
