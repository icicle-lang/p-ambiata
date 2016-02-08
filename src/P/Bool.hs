{-# LANGUAGE CPP #-}
module P.Bool (
    whenM
  , unlessM
  , ifM
  , guardM
#if __GLASGOW_HASKELL__ >= 708
  , Data.Bool.bool
#else
  , bool
#endif
  ) where

import           Control.Monad (MonadPlus, when, unless, guard)

#if __GLASGOW_HASKELL__ >= 708
import qualified Data.Bool
#else
bool :: a -> a -> Bool -> a
bool f t p = if p then t else f
#endif

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y =
  p >>= \b -> if b then x else y

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f
