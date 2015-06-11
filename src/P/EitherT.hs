module P.EitherT (
    EitherT(..)
  , eitherT
  , bimapEitherT
  , left
  , hoistEither
  , mapEitherT
  , swapEitherT
  , firstEitherT
  , secondEitherT
  , eitherTFromMaybe
  ) where

import           Control.Monad.Trans.Class ( MonadTrans(..) )
import           Control.Monad.Trans.Either
import           Data.Functor ()

firstEitherT :: (Functor m) => (e -> f) -> EitherT e m a -> EitherT f m a
firstEitherT f = bimapEitherT f id

secondEitherT :: (Functor m) => (a -> b) -> EitherT e m a -> EitherT e m b
secondEitherT = bimapEitherT id

eitherTFromMaybe :: (Monad m) => b -> m (Maybe a) -> EitherT b m a
eitherTFromMaybe l r = lift r >>= maybe (left l) return
