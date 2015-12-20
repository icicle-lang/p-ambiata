{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.P.Applicative where

import           Control.Applicative ((<$>), pure)

import           Data.Bool (Bool(..))
import           Data.Function (($))
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int)
import           Data.Maybe (Maybe(..))
import           Data.Monoid (Sum(..))

import           P.Applicative

import           Prelude (Eq(..))

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Property.Monoid (prop_Monoid, T(..))
import           Test.QuickCheck.Property.Common (eq)

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

prop_applicative_monoid = eq $ prop_Monoid (T :: T (ApplicativeMonoid Identity (Sum Int)))

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary (m a) => Arbitrary (ApplicativeMonoid m a) where
  arbitrary = ApplicativeMonoid <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

pure []
tests :: IO Bool
tests = $quickCheckAll
