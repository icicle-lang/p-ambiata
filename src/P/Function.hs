{-# LANGUAGE NoImplicitPrelude #-}
module P.Function (
    id
  , const
  , (.)
  , flip
  , ($)
  , fix
  , on
  , applyN
  ) where

import           Data.Int (Int)
import           Data.List (foldr, replicate)
import           Data.Function (id, const, (.), flip, ($))
import           Data.Function (fix, on)

applyN :: Int -> (a -> a) -> a -> a
applyN n f =
  foldr (.) id (replicate n f)
