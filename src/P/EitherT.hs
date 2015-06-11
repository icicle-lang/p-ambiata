module P.EitherT (
    firstEitherT
  , secondEitherT
  , eitherTFromMaybe
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Data.Functor ()

firstEitherT :: (Functor m) => (e -> f) -> EitherT e m a -> EitherT f m a
firstEitherT f = bimapEitherT f id

secondEitherT :: (Functor m) => (a -> b) -> EitherT e m a -> EitherT e m b
secondEitherT f = bimapEitherT id f

eitherTFromMaybe :: Functor m => b -> m (Maybe a) -> EitherT b m a
eitherTFromMaybe l r = EitherT $ maybe (Left l) pure <$> r
