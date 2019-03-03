{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module P.Monoid (
  -- * 'Monoid' typeclass
    Monoid(..)
  , Semigroup(..)

  -- * Extensions
  , valueOrZero
  , zeroOrValue
  , valueOrZeroM
  ) where

import           Control.Applicative (Applicative(..))

import           Data.Semigroup (Semigroup(..))
import           Data.Monoid (Monoid(..))

import           Data.Bool (Bool, not)
import           Data.Function ((.))

valueOrZero :: Monoid a => Bool -> a -> a
valueOrZero b a =
  if b then a else mempty

zeroOrValue :: Monoid a => Bool -> a -> a
zeroOrValue =
  valueOrZero . not

valueOrZeroM :: (Applicative f, Monoid a) => Bool -> f a -> f a
valueOrZeroM b a =
  if b then a else pure mempty
