module P.Maybe (
    fromMaybeM
  , lazyMaybe'
  , strictMaybe
  ) where

import           Control.Applicative

import           P.Maybe.Strict

import           Prelude

fromMaybeM :: Applicative f => f a -> Maybe a -> f a
fromMaybeM = flip maybe pure


strictMaybe :: Maybe a -> Maybe' a
strictMaybe Nothing = Nothing'
strictMaybe (Just x) = Just' x

lazyMaybe' :: Maybe' a -> Maybe a
lazyMaybe' Nothing' = Nothing
lazyMaybe' (Just' x) = Just x
