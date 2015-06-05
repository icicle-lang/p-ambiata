module P.EitherT (
    firstEitherT
  , secondEitherT
  ) where

import           Control.Monad.Trans.Either
import           Data.Functor ()

firstEitherT :: (Functor m) => (e -> f) -> EitherT e m a -> EitherT f m a
firstEitherT f = bimapEitherT f id

secondEitherT :: (Functor m) => (a -> b) -> EitherT e m a -> EitherT e m b
secondEitherT f = bimapEitherT id f
