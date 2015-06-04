module P.Applicative (
    valueOrEmpty
  , emptyOrValue
  , orEmpty
  ) where

import           Control.Applicative
import           Data.Monoid


valueOrEmpty :: Alternative f => Bool -> a -> f a
valueOrEmpty b a = if b then pure a else empty

emptyOrValue :: Alternative f => Bool -> a -> f a
emptyOrValue = valueOrEmpty . not

orEmpty :: (Alternative f, Monoid a) => f a -> f a
orEmpty f = f <|> pure mempty
