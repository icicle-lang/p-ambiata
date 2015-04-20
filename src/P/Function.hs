{-# LANGUAGE NoImplicitPrelude #-}
module P.Function (
    applyN
  ) where

import           Data.Int
import           Data.List
import           Data.Function ((.), id)

applyN :: Int -> (a -> a) -> a -> a
applyN n f =
  foldr (.) id (replicate n f)
