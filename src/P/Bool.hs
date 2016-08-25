{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.Bool (
    andA
  , orA
  , whenM
  , unlessM
  , ifM
  , guardM
#if __GLASGOW_HASKELL__ >= 708
  , Data.Bool.bool
#else
  , bool
#endif
  ) where

import           Control.Applicative (Applicative, liftA2)
import           Control.Monad (Monad(..), MonadPlus, (=<<), when, unless, guard)

import           Data.Bool (Bool, (&&), (||))
import           Data.Function (flip)

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

-- | Logical disjunction.
orA :: Applicative f => f Bool -> f Bool -> f Bool
orA =
  liftA2 (||)

-- | Logical conjunction.
andA :: Applicative f => f Bool -> f Bool -> f Bool
andA =
  liftA2 (&&)

infixl 8 `andA`, `orA`
