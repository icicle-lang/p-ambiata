{-# LANGUAGE CPP #-}

module P (
    module X
  ) where

import           Prelude as X (
                     Enum
                   , Bounded
                   , minBound
                   , maxBound
                   , ($!)
                   , seq
                   )

import           P.Applicative as X
import           P.Bifunctor.Trans as X
import           P.Bool as X
import           P.Either as X
import           P.Foldable as X
import           P.Functor as X
import           P.Maybe as X
import           P.Maybe.Strict as X
import           P.Monad as X
import           P.Monoid as X
import           P.Ord as X
import           P.List as X
import           P.Function as X
import           P.Debug as X
import           P.Show as X
import           Control.Applicative as X (
                     Applicative(..)
                   , Alternative(..)
                   , Const(..)
                   , WrappedMonad(..)
                   , WrappedArrow(..)
                   , ZipList(..)
                   , (<**>)
                   , liftA
                   , liftA2
                   , liftA3
                   , optional
                   )
import           Control.DeepSeq as X (
                     NFData(..)
                   , ($!!)
                   , deepseq
                   , force
                   )
import           Data.Eq as X
import           Data.Bifunctor as X (Bifunctor(..))
import           Data.Bool as X
import           Data.Char as X (Char)
import           Data.List as X (
                     intercalate
                   , isPrefixOf
                   , drop
                   , splitAt
                   , break
                   , filter
                   , reverse
#if (__GLASGOW_HASKELL__ < 710)
                   , length
                   , null
#endif
                   )
import           Data.Maybe as X hiding (fromJust)
import           Data.Either as X
import           Data.Int as X
import           Data.Ord as X
import           Data.Tuple as X
import           Data.Traversable as X
import           Data.Text as X (Text)
import           Data.Foldable as X hiding (
                     foldr1
                   , foldl1
                   , maximum
                   , maximumBy
                   , minimum
                   , minimumBy
                   )
import           GHC.Num as X
import           GHC.Real as X
import           GHC.Float as X
import           Text.Show as X
import           Text.Read as X (Read, reads, readMaybe, readEither)
