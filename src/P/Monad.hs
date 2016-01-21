{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.Monad (
  -- * Functor and monad classes
    Monad(..)
  , MonadPlus(..)

  -- * Functions
  -- ** Basic @Monad@ functions
  , (=<<)
  , (>=>)
  , (<=<)
  , forever

  -- ** Generalisations of list functions
  , join
  , mfilter
  , filterM
  , mapAndUnzipM
  , zipWithM
  , zipWithM_
  , foldM
  , foldM_
  , replicateM
  , replicateM_

  -- ** Conditional execution of monadic expressions
  , guard
  , when
  , unless

  -- ** Monadic lifting operators
  , liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  , ap

  -- * Strict monadic functions
  , (<$!>)
  ) where

import           Control.Monad (Monad(..), MonadPlus(..))
import           Control.Monad ((=<<), (>=>), (<=<), forever)
import           Control.Monad (join, mfilter, filterM, mapAndUnzipM, zipWithM, zipWithM_)
import           Control.Monad (foldM, foldM_, replicateM, replicateM_)
import           Control.Monad (guard, when, unless)
import           Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5, ap)

#if (__GLASGOW_HASKELL__ >= 710)

import           Control.Monad ((<$!>))

#else

import           Prelude (seq)

infixl 4 <$!>

-- | Strict version of 'Data.Functor.<$>'.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}

#endif
