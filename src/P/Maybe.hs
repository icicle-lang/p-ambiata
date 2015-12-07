module P.Maybe (
    fromMaybeM
  ) where

import           Control.Applicative
import           Prelude

fromMaybeM :: Applicative f => f a -> Maybe a -> f a
fromMaybeM = flip maybe pure
