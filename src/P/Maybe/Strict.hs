{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.Maybe.Strict (
    Maybe'(..)
  , fromMaybe'
  , fromMaybeM'
  , isJust'
  , isNothing'
  , maybe'
  ) where

import           Control.Applicative (Applicative(..), Alternative(..))
import           Control.DeepSeq (NFData(..), rnf)
import           Control.Monad (Monad(..), MonadPlus(..))

import           Data.Bool (Bool(..))
import           Data.Data (Data)
import           Data.Eq (Eq)
import           Data.Foldable (Foldable)
import           Data.Function (flip)
import           Data.Functor (Functor(..))
import           Data.Ord (Ord)
import           Data.Traversable (Traversable)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           Text.Show (Show)
import           Text.Read (Read)

-- | Strict version of 'Data.Maybe.Maybe'.
data Maybe' a =
    Just' !a
  | Nothing'
  deriving (Eq, Ord, Read, Show, Foldable, Traversable, Generic, Data, Typeable)

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'

  Just' f <*> m = fmap f m
  Nothing' <*> _m = Nothing'

  Just' _m1 *> m2 = m2
  Nothing' *> _m2 = Nothing'

instance Alternative Maybe' where
  empty =
    Nothing'

  (<|>) l r =
    case l of
      Nothing' ->
        r
      Just' _ ->
        l

instance MonadPlus Maybe' where
  mzero =
    empty

  mplus =
    (<|>)

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
