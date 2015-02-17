module P.Applicative (
    valueOrEmpty
  , emptyOrValue
  , zipA2
  ) where

import           Control.Applicative


valueOrEmpty :: Alternative f => Bool -> a -> f a
valueOrEmpty b a = if b then pure a else empty

emptyOrValue :: Alternative f => Bool -> a -> f a
emptyOrValue = valueOrEmpty . not

zipA2 :: Applicative f => f a -> f b -> f (a, b)
zipA2 = liftA2 (,)
