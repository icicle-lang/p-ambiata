{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
module P.Bifunctor.Trans (
    BifunctorTrans(..)
  ) where

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import           Data.Either (Either(..))
import           Data.Function ((.), id)
import           Data.Functor (Functor(..))

------------------------------------------------------------------------

-- | You can define a 'BifunctorTrans' by either defining 'bimapT' or by
--   defining both 'firstT' and 'secondT'.
--
--   If you supply 'bimapT', you should ensure that:
--
--   @'bimapT' 'id' 'id' == 'id'@
--
--   If you supply 'first' and 'second', ensure:
--
--   @
--   'firstT'  'id' == 'id'
--   'secondT' 'id' == 'id'
--   @
--
--   If you supply both, you should also ensure:
--
--   @'bimapT' f g == 'firstT' f '.' 'secondT' g@
--
--   These ensure by parametricity:
--
--   @
--   'bimapT'  (f '.' g) (h '.' i) == 'bimapT' f h '.' 'bimapT' g i
--   'firstT'  (f '.' g) == 'firstT'  f '.' 'firstT'  g
--   'secondT' (f '.' g) == 'secondT' f '.' 'secondT' g
--   @
class BifunctorTrans (t :: * -> (* -> *) -> * -> *) where
  bimapT :: Functor f => (x -> y) -> (a -> b) -> t x f a -> t y f b
  bimapT f g =
    firstT f . secondT g
  {-# INLINE bimapT #-}

  firstT :: Functor f => (x -> y) -> t x f a -> t y f a
  firstT f =
    bimapT f id
  {-# INLINE firstT #-}

  secondT :: Functor f => (a -> b) -> t x f a -> t x f b
  secondT =
    bimapT id
  {-# INLINE secondT #-}

  {-# MINIMAL bimapT | firstT, secondT #-}

------------------------------------------------------------------------

instance BifunctorTrans ExceptT where
  bimapT f g =
    let h (Left  x) = Left  (f x)
        h (Right a) = Right (g a)
        {-# INLINE h #-}
    in ExceptT . fmap h . runExceptT
  {-# INLINE bimapT #-}

instance BifunctorTrans Lazy.WriterT where
  bimapT f g =
    let h (a, x) = (g a, f x)
        {-# INLINE h #-}
    in Lazy.WriterT . fmap h . Lazy.runWriterT
  {-# INLINE bimapT #-}

instance BifunctorTrans Strict.WriterT where
  bimapT f g =
    let h (a, x) = (g a, f x)
        {-# INLINE h #-}
    in Strict.WriterT . fmap h . Strict.runWriterT
  {-# INLINE bimapT #-}
