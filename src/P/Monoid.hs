module P.Monoid (
    valueOrZero
  , zeroOrValue
  ) where

import           Data.Monoid


valueOrZero :: Monoid a => Bool -> a -> a
valueOrZero b a = if b then a else mempty

zeroOrValue :: Monoid a => Bool -> a -> a
zeroOrValue = valueOrZero . not
