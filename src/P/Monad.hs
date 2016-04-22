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
  , join
  , forever

  -- ** Generalisations of list functions
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
  , (>>=!!)
  , (>>=!)

  -- * Extensions
  , bind
  , liftM'
  , liftM2'
  ) where

import           Control.Monad (Monad(..), MonadPlus(..))
import           Control.Monad ((=<<), (>=>), (<=<), forever)
import           Control.Monad (join, mfilter, filterM, mapAndUnzipM, zipWithM, zipWithM_)
import           Control.Monad (foldM, foldM_, replicateM, replicateM_)
import           Control.Monad (guard, when, unless)
import           Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5, ap)

import           Control.DeepSeq (NFData, force)
import           Prelude (seq, (.))

#if (__GLASGOW_HASKELL__ >= 710)

import           Control.Monad ((<$!>))

#else

infixl 4 <$!>

-- | Strict version of 'Data.Functor.<$>'.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}

#endif

infixl 1 >>=!!

-- | Deepseq version 'Control.Monad.>>='
(>>=!!) :: (NFData a, Monad m) => m a -> (a -> m b) -> m b
(>>=!!) m f = m >>= f . force
{-# INLINE (>>=!!) #-}

-- | Seq version 'Control.Monad.>>='
(>>=!) :: (Monad m) => m a -> (a -> m b) -> m b
(>>=!) m f = m >>= f . (\x -> x `seq` x)
{-# INLINE (>>=!) #-}

-- | Identifier version of 'Control.Monad.=<<'.
bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)
{-# INLINE bind #-}

-- | Strict version of 'Control.Monad.liftM'.
liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' = (<$!>)
{-# INLINE liftM' #-}

-- | Strict version of 'Control.Monad.liftM2'.
liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  x <- a
  y <- b
  let z = f x y
  z `seq` return z
{-# INLINE liftM2' #-}
