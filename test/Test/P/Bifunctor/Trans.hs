{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.P.Bifunctor.Trans where

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import           Data.Tuple (swap)

import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Test.P.Bifunctor.Trans.Laws


prop_bifunctor_ExceptT x f g =
  bifunctorLaws
    ExceptT
    runExceptT
    (x :: Either Int Int)
    (f :: Fun Int Int)
    (g :: Fun Int Int)

prop_bifunctor_StrictWriterT x f g =
  bifunctorLaws
    (Strict.WriterT . fmap swap)
    (fmap swap . Strict.runWriterT)
    (x :: (Int, Int))
    (f :: Fun Int Int)
    (g :: Fun Int Int)

prop_bifunctor_LazyWriterT x f g =
  bifunctorLaws
    (Lazy.WriterT . fmap swap)
    (fmap swap . Lazy.runWriterT)
    (x :: (Int, Int))
    (f :: Fun Int Int)
    (g :: Fun Int Int)

return []
tests :: IO Bool
tests = $quickCheckAll
