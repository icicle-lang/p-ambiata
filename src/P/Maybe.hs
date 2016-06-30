module P.Maybe (
    fromMaybeM
  , lazyMaybe'
  , strictMaybe
  , mcase
  , mcase'
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

mcase :: Maybe a -> b -> (a -> b) -> b
mcase m b = flip (maybe b) m

mcase' :: Maybe' a -> b -> (a -> b) -> b
mcase' m b = flip (maybe' b) m
