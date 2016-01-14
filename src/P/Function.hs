{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module P.Function (
  -- * Prelude re-exports
    id
  , const
  , (.)
  , flip
  , ($)

  -- * Other combinators
  , (&)
  , fix
  , on

  -- * Extensions
  , applyN
  ) where

import           Data.Int (Int)
import           Data.List (foldr, replicate)
import           Data.Function (id, const, (.), flip, ($))
import           Data.Function (fix, on)

#if (__GLASGOW_HASKELL__ >= 710)

import           Data.Function ((&))

#else

infixl 1 &

-- | '&' is a reverse application operator.  This provides notational
--   convenience.  Its precedence is one higher than that of the forward
--   application operator '$', which allows '&' to be nested in '$'.
(&) :: a -> (a -> b) -> b
x & f =
  f x

#endif

applyN :: Int -> (a -> a) -> a -> a
applyN n f =
  foldr (.) id (replicate n f)
