{-# LANGUAGE NoImplicitPrelude #-}
module P.Functor (
    module X
  , with
  ) where

import           Data.Function (flip)
import           Data.Functor as X (Functor(..), ($>), (<$>), void)

with :: Functor f => f a -> (a -> b) -> f b
with =
  flip fmap
