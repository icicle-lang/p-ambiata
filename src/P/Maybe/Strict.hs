{-# LANGUAGE NoImplicitPrelude #-}
module P.Maybe.Strict (
    Maybe'(..)
  , fromMaybe'
  , isJust'
  , isNothing'
  ) where

import           Control.DeepSeq (NFData(..), rnf)

import           Data.Bool (Bool(..))
import           Data.Eq (Eq(..))
import           Data.Functor (Functor(..))

import           Prelude (Show(..))

-- | Strict version of 'Data.Maybe.Maybe'.
data Maybe' a =
    Just' !a
  | Nothing'
  deriving (Eq, Show)

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

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
