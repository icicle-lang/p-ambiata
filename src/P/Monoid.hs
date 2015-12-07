module P.Monoid (
    valueOrZero
  , zeroOrValue
  , valueOrZeroM
  ) where

import           Control.Applicative
import           Data.Monoid

import           Prelude

valueOrZero :: Monoid a => Bool -> a -> a
valueOrZero b a =
  if b then a else mempty

zeroOrValue :: Monoid a => Bool -> a -> a
zeroOrValue =
  valueOrZero . not

valueOrZeroM :: (Applicative f, Monoid a) => Bool -> f a -> f a
valueOrZeroM b a =
  if b then a else pure mempty
