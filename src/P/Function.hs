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

  -- * Composition
  , (...)
  , (....)
  , (.....)
  , (......)

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

-- | Compose a unary function with a binary function.
(...) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(...) = (.) . (.)

-- | Compose a unary function with a ternary function.
(....) :: (a -> b) -> (c -> d -> e -> a) -> c -> d -> e -> b
(....) = (.) . (...)

-- | Compose a unary function with a quaternary function.
(.....) :: (a -> b) -> (c -> d -> e -> f -> a) -> c -> d -> e -> f -> b
(.....) = (.) . (....)

-- | Compose a unary function with a quintic function.
(......) :: (a -> b) -> (c -> d -> e -> f -> g -> a) -> c -> d -> e -> f -> g -> b
(......) = (.) . (.....)

applyN :: Int -> (a -> a) -> a -> a
applyN n f =
  foldr (.) id (replicate n f)
