{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module P.Monoid (
  -- * 'Monoid' typeclass
    Monoid(..)
  , (<>)
  , Dual(..)
  , Endo(..)

  -- * 'Bool' wrappers
  , All(..)
  , Any(..)

  -- * 'Num' wrappers
  , Sum(..)
  , Product(..)

  -- * 'Maybe' wrappers
  , First(..)
  , Last(..)

  -- * 'Alternative' wrapper
  , Alt(..)

  -- * Extensions
  , valueOrZero
  , zeroOrValue
  , valueOrZeroM
  ) where

import           Control.Applicative (Applicative(..))

import           Data.Bool (Bool, not)
import           Data.Function ((.))
import           Data.Monoid (All(..), Any(..))
import           Data.Monoid (First(..), Last(..))
import           Data.Monoid (Monoid(..), Dual(..), Endo(..), (<>))
import           Data.Monoid (Sum(..), Product(..))

#if (__GLASGOW_HASKELL__ >= 710)

import           Data.Monoid (Alt(..))

#else

import           Control.Applicative (Alternative(..))
import           Control.Monad (Monad, MonadPlus)

import           Data.Coerce (coerce)
import           Data.Eq (Eq)
import           Data.Functor (Functor)
import           Data.Ord (Ord)

import           GHC.Generics (Generic, Generic1)

import           Prelude (Num, Enum)

import           Text.Read (Read)
import           Text.Show (Show)

-- | Monoid under '<|>'.
--
-- @since 4.8.0.0
newtype Alt f a =
  Alt {
      getAlt :: f a
    } deriving (Generic, Generic1, Read, Show, Eq, Ord, Num, Enum, Monad, MonadPlus, Applicative, Alternative, Functor)

instance forall f a . Alternative f => Monoid (Alt f a) where
  mempty =
    Alt empty
  mappend =
    coerce ((<|>) :: f a -> f a -> f a)

#endif

valueOrZero :: Monoid a => Bool -> a -> a
valueOrZero b a =
  if b then a else mempty

zeroOrValue :: Monoid a => Bool -> a -> a
zeroOrValue =
  valueOrZero . not

valueOrZeroM :: (Applicative f, Monoid a) => Bool -> f a -> f a
valueOrZeroM b a =
  if b then a else pure mempty
