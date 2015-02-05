module P.Maybe (
    fromMaybeM
  ) where

import           Control.Applicative


fromMaybeM :: Applicative f => f a -> Maybe a -> f a
fromMaybeM = flip maybe pure
