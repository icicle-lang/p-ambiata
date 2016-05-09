{-# LANGUAGE NoImplicitPrelude #-}
module P.Maybe.Strict (
    Maybe'(..)
  , fromMaybe'
  , fromMaybeM'
  , isJust'
  , isNothing'
  , maybe'
  ) where

import           Control.Applicative (Applicative(..))
import           Control.DeepSeq (NFData(..), rnf)
import           Control.Monad (Monad(..))

import           Data.Bool (Bool(..))
import           Data.Eq (Eq(..))
import           Data.Functor (Functor(..))

import           Prelude (Show(..), flip)

-- | Strict version of 'Data.Maybe.Maybe'.
data Maybe' a =
    Just' !a
  | Nothing'
  deriving (Eq, Show)

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'

  Just' f <*> m = fmap f m
  Nothing' <*> _m = Nothing'

  Just' _m1 *> m2 = m2
  Nothing' *> _m2 = Nothing'

-- | Not technically a monad due to bottom, but included anyway as we don't
-- use partial functions.
instance Monad Maybe' where
  (Just' x) >>= k = k x
  Nothing' >>= _ = Nothing'

  (>>) = (*>)

  return = Just'

  fail _ = Nothing'

instance NFData a => NFData (Maybe' a) where
  rnf Nothing' = ()
  rnf (Just' x) = rnf x

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' n _ Nothing' = n
maybe' _ f (Just' x) = f x

isJust' :: Maybe' a -> Bool
isJust' Nothing' = False
isJust' (Just' _) = True

isNothing' :: Maybe' a -> Bool
isNothing' Nothing' = True
isNothing' (Just' _) = False

fromMaybe' :: a -> Maybe' a -> a
fromMaybe' x Nothing' = x
fromMaybe' _ (Just' y) = y

fromMaybeM' :: Applicative f => f a -> Maybe' a -> f a
fromMaybeM' = flip maybe' pure
