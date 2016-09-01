{-# LANGUAGE NoImplicitPrelude #-}
module P.Functor (
    module X
  , with
  ) where

import           Data.Functor as X (Functor(..), ($>), (<$>), void)

with :: Functor f => f a -> (a -> b) -> f b
with xs f =
  fmap f xs
{-# INLINE with #-}
