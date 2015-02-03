module P.Applicative (
    valueOrEmpty
  , emptyOrValue
  ) where

import           Control.Applicative


valueOrEmpty :: Alternative f => Bool -> a -> f a
valueOrEmpty b a = if b then pure a else empty

emptyOrValue :: Alternative f => Bool -> a -> f a
emptyOrValue = valueOrEmpty . not
