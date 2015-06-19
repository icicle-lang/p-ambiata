{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.P.Applicative where

import           P.Applicative
import           Control.Applicative
import           Data.Monoid
import           Data.Functor.Identity
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

return []
tests :: IO Bool
tests = $quickCheckAll
