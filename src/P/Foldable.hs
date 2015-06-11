{-# LANGUAGE NoImplicitPrelude #-}
module P.Foldable (
    findMapM
  , head
  ) where

import           Control.Monad

import           Data.Foldable
import           Data.Function ((.))
import           Data.Maybe

findMapM :: (Monad m, Foldable f) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
findMapM f = foldr (\a a' -> f a >>= maybe a' (return . Just)) (return Nothing)

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> return x) Nothing
