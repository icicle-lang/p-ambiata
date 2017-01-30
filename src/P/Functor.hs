{-# LANGUAGE NoImplicitPrelude #-}
module P.Functor (
    module X
  , with
  , (<$$>)
  , fmap2
  ) where

import           Data.Functor as X (Functor(..), ($>), (<$>), void)
import           Data.Function((.))

with :: Functor f => f a -> (a -> b) -> f b
with xs f =
  fmap f xs
{-# INLINE with #-}

(<$$>) :: (Functor g, Functor f) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
{-# INLINE (<$$>) #-}

fmap2 :: (Functor g, Functor f) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
{-# INLINE fmap2 #-}
